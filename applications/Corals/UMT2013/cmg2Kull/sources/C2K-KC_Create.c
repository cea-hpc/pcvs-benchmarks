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
#include <assert.h>
#include <string.h>

#ifdef __APPLE__
#include <Math.h>
#else
#include <math.h>
#endif

#include "C2K-Storage.h"
#include "C2K-Lists.h"
#include "C2K-CMG.h"
#include "C2K-KC_Create.h"
#include "C2K-KC_Info.h"
#include "C2K-KC_API.h"
#include "C2K-KC_SubDivide.h"

#ifdef USE_MPI
#include "mpi.h"
#endif

#define HEX 0
#define TET 1
#define PRI 2
#define PYR 3

//================================================================================
// These are critical definitions of how the four basic finite element zone types
// are constructed.
//                     HEX  TET  PRI  PYR
int	EdgesPerZone[4] = { 12,   6,   9,   8};
int	NodesPerZone[4] = {  8,   4,   6,   5};
int	FacesPerZone[4] = {  6,   4,   5,   5};

										// These are 0-origin indices
int	ByZoneTypeEdgeNodes[4][12][2] = {{{0,4},{1,5},{2,6},{3,7},{0,1},{4,5},	// HEX
							          {7,6},{3,2},{0,3},{1,2},{5,6},{4,7}},
							         {{0,1},{0,2},{0,3},{1,2},{1,3},{2,3},	// TET
							          {0,0},{0,0},{0,0},{0,0},{0,0},{0,0}},
							         {{0,1},{0,2},{0,3},{1,2},{1,4},{2,5},	// PRI
							          {3,4},{3,5},{4,5},{0,0},{0,0},{0,0}},
							         {{0,1},{0,3},{0,4},{1,2},{1,4},{2,3},	// PYR
							          {2,4},{3,4},{0,0},{0,0},{0,0},{0,0}} };

int ByZoneTypeByFaceNumOfNodes[4][6] = {{4,4,4,4,4,4},{3,3,3,3,0,0},{3,4,4,4,3,0},{4,3,3,3,3,0}};

									// These are 0-origin indices, in CCW order, wrt the Zone center
int ByZoneTypeFaceNodes[4][6][4] = {{{1,2,3,0},{4,5,1,0},{3,7,4,0},{5,6,2,1},{6,7,3,2},{7,6,5,4}},
									{{1,2,0,0},{3,1,0,0},{3,2,1,0},{2,3,0,0},{0,0,0,0},{0,0,0,0}},
									{{1,2,0,0},{3,4,1,0},{4,5,2,1},{2,5,3,0},{5,4,3,0},{0,0,0,0}},
									{{1,2,3,0},{4,1,0,0},{4,2,1,0},{4,3,2,0},{3,4,0,0},{0,0,0,0}} };

//================================================================================

int	cmgT2KC ( cZoneType cZType);// prototype; convert CMG zone type to Kull connectivity (KC) Zone type

#define NO  0
#define YES 1
#define MAX_FACES_AT_NODE 48
#define MAX_SIDES_ON_FACE 4
#define MAX_SIDES_AT_EDGE 24
// The maximum # of communication neighbors
#define MAX_CN 6	

//------------------------------------------------------------ Storage for local to global mappings
int	N_Total;

#ifdef CMG_FAKE
extern int N_Offset;	// the total # of Nodes, the start in GNID of UNOWNED Nodes
#else
int N_Offset;
#endif
						// from 0 to N_Offset-1, GIDs are sequential. from N_Offset on, they are unordered
int		Owned_Low, Owned_High;	// the index limits in GNID of OWNED Nodes
//--------------------------------------------------------------------------------
int		Z_Total;				// the total # of Zones
//--------------------------------------------------------------------------------
int		NumCommN;				// the number of COMM neighbor domains
int		CBF_Total;				// the total number of COMM boundary Faces
//--------------------------------------------------------------------------------
int		DBF_Total = 0;			// the total number of DOMAIN boundary Faces

// the global ID of a local Node */
int** getGNID(){
  static int* gGNID = NULL;
  return &gGNID;
}

// the global ID of a local Zone
int **getGZID(){
  static int* gGZID = NULL;
  return &gGZID;
}

// given a local Zone ID, provide the LOCAL Node IDs for the Zone
int ***getLZsLNs(){
  static int** gLZsLNs = NULL;
  return &gLZsLNs;
}

// the KC Zone type for the Zone
int **getLZsKC_Type(){
  static int* gLZsKC_Type = NULL;
  return &gLZsKC_Type;
}

// to hold the domain IDs of COMM neighbors
int **getCN_DomainID(){
  static int* gCN_DomainID = NULL;
  return &gCN_DomainID;
}

// to hold the # of Faces for each COMM neighbor
int **getCN_NumFaces(){
  static int* gCN_NumFaces=NULL;
  return &gCN_NumFaces;
}

// the global IDs of all COMM boundary Faces [MAX_FACES IS TOO BIG => dynamic]
int **getCBF_GID(){
  static int* gCBF_GID = NULL ;
  return &gCBF_GID;
}

// the Local IDs of all COMM boundary Faces  [future optimization needed]
int **getCBF_LID(){
  static int* gCBF_LID = NULL;
  return &gCBF_LID;
}

// the global IDs of all DOMAIN boundary Faces
int **getDBF_GID(){
  static int* gDBF_GID = NULL;
  return &gDBF_GID;
}

// the Local IDs of all DOMAIN boundary Faces
int **getDBF_LID(){
  static int* gDBF_LID = NULL;
  return &gDBF_LID;
}



//================================================================================
void debugStorage ( )
{	int		ix, nN, nZ;
	NodePtr	pN;
	ZonePtr	pZ;
	
	nN = getNumN ( );
	if ((nN < 0) || (nN > getNumN()))
	{	printf("DEBUG Bad number of Nodes. numNodes = %d\n", nN);
		exit(1);
	}
	printf("DEBUG Node Storage Status. numNodes = %d\n", nN);
	printf("DEBUG NodeID,   x,          y,          z,     pNode\n");
	for (ix = 0; ix < nN; ix++)
	{	pN = pNode( ix );
		printf ("%8d, %10.1e  %10.1e  %10.1e  %p\n",
			pN->nNodeLID, pN->Loc3D.x, pN->Loc3D.y, pN->Loc3D.z, pN);
	}

	nZ = getNumZ ( );
	if ((nZ < 0) || (nZ > getNumN()))
	{	printf("DEBUG Bad number of Zones. numZones = %d\n", nZ);
		exit(1);
	}
	printf("DEBUG Zone Storage Status. numZones = %d\n", nZ);
	printf("DEBUG ZoneID, pZone\n");
	for (ix = 0; ix < nZ; ix++)
	{	pZ = pZone( ix );
		printf ("%8d,     %p\n",
				pZ->nZoneLID, pZ);
	}

}

//======================================================================
int CreateRealNodes ( )
{	
	int		numN, iN;
	NodePtr	pN;
	double	xN, yN, zN;
	int **pGNID;
  int *GNID;
  pGNID = getGNID();
  GNID = *pGNID;
  
	cnmnoda ( &numN );				// ask CMG how many nodes there are, OWNED & UNOWNED
	N_Total = numN;					// store for use throughout C2K-Geom.c
	
	//------------------------------ fill the array of global Node IDs
	cnodsa ( &GNID[0], &N_Offset);
	Owned_Low  = 0;					// prepare to allow conversion from global to local Node IDs
	Owned_High = N_Offset - 1;
/*   printf("NODE list.\n"); */
  
	for (iN = 0; iN < numN; iN++)	// for each CMG node, ...
	{
		pN = allocateNode ( iN );		// create a KC Node  (Note! KC means Kull connectivity), ...
		
		cnodpos (GNID[iN], &xN, &yN, &zN);// get the 3D CMG node position, ...
		
		pN->Loc3D.x = xN;			// and store the CMG position in the KC Node
		pN->Loc3D.y = yN;			// 
		pN->Loc3D.z = zN;			//
		pN->nNodeGID = GNID[iN];	// store the global Node ID
/*     printf("GID=%d, LID=%d (%5.3f,%5.3f,%5.3f)\n",pN->nNodeGID,pN->nNodeLID,xN,yN,zN); */
		
		if (pN->nNodeLID != iN)
		{	printf("ERROR in allocation of KC storage for Nodes.\n");
			exit(1);
		}
	}
/* 	printf("CreateRealNodes:\n    Created %3d Owned Nodes.\n", numN); */

	return ( numN );
} // End of CreateRealNodes

//======================================================================
int G2LN (int GID)	//----- convert global Node ID to local Node ID
					//      OK to call ONLY AFTER CreateRealNodes has been called
{
  /* search through the global node list for the index of the input GID,
     now it's an n^2 algorithm. */
  int iN;
  int **pGNID;
  int *GNID;
  pGNID= getGNID();
  GNID = *pGNID;
  
  for (iN = 0; iN < N_Total; iN++)
    if (GID == GNID[iN]) return (iN);

  return -100; /* error condition? */

}

//======================================================================
int CreateRealZones ( )
{	
	int			numZ, iZ;
	ZonePtr		pZ;
	int			GN[8];		// the global node IDs of a zone
	int			iN;			// loop index
	cZoneType	typ;		// CMG's zone type
  int ***pLZsLNs;
	int   **pGZID, **pLZsKC_Type,**LZsLNs;
  int   *GZID,*LZsKC_Type;
  
  pGZID = getGZID();
  GZID = *pGZID;
  pLZsLNs = getLZsLNs();
  LZsLNs = *pLZsLNs;
  pLZsKC_Type = getLZsKC_Type();
  LZsKC_Type = *pLZsKC_Type;
  
	cnumzns ( &numZ );				// ask CMG how many zones there are
	Z_Total = numZ;					// store for use throughout C2K-Geom.c
	
	czns ( &GZID[0] );				// get global IDs for all zones
	
	for (iZ = 0; iZ < numZ; iZ++)	// for each CMG zone, ...
	{
		pZ = allocateZone ( iZ );		// create a KC Zone, ...
		
		//---------------------------------------- convert CMG's zonal node table for access
		//										   using local Zone IDs returning local Node IDs
		cgetznn (GZID[iZ], &GN[0]);
		
		for (iN = 0; iN < 8; iN++)
			LZsLNs[iZ][iN] = G2LN ( GN[iN] );	// global to local node ID conversion
		
/* 		cgetztp (GZID[iZ], & typ);				// CMG's zone type */
		cgetztp (iZ, & typ);				// CMG's zone type, indexed by local zone id 
		
		LZsKC_Type[iZ] = cmgT2KC ( typ );		// convert to KC zone type, and store it

		//---------------------------------------- end of conversions from CMG and global Node IDs
		
		if (pZ->nZoneLID != iZ)
		{	printf("ERROR in allocation of KC storage for Zones.\n");
			exit(1);
		}
	}
/* 	printf("CreateRealZones:\n    Created %3d Owned Zones.\n", numZ); */
	
	return ( numZ );
} // End of CreateRealZones

//======================================================================
int	cmgT2KC ( cZoneType cZType)
{	int	KCType;

	if		( cZType == CMG_HEX ) KCType = HEX;
	else if ( cZType == CMG_TET ) KCType = TET;
	else if ( cZType == CMG_PRI ) KCType = PRI;
	else if ( cZType == CMG_PYR ) KCType = PYR;
	else
	{	printf("ERROR Unknown zone type.\n");
		exit(1);
	}
	return ( KCType );
}

//======================================================================
int getIndexInEdgeListWithSameNode2 (LItemPtr pLEI, NodePtr pN_High )	// used in CreateRealEdges
{	EdgePtr		pECheck;
	int			numLI, iLI;
	
	numLI = CountListItems ( pLEI );
	for (iLI = 0; iLI < numLI; iLI++)
	{
		pECheck = RetrieveListItem (pLEI, umEDGE, iLI);
		if (pECheck->pNode2 == pN_High) return (iLI);	// Edgefound, => already created
	}
	return (-1);	// This signals that the Edge was not found.
}

//======================================================================
int CreateRealEdges ( )
{
	int			numE = 0, iE, numZ, iZ, iLow, iHigh, Create_Edge, EdgesInZone, iN;
	EdgePtr		pE;
	int			KCZType;
	int			EsN1, EsN2;			// NodeID
	int			ZsNodeID[8];		// Zone's ORDERED NodeIDs
	LItemPtr	pLEI;				// a pointer to a list of Edge items
	NodePtr		pN_Low, pN_High;
	int       *LZsKC_Type;
  int      **pLZsKC_Type,**LZsLNs;
  int      ***pLZsLNs;
  
  pLZsLNs = getLZsLNs();
  LZsLNs = *pLZsLNs;
  pLZsKC_Type = getLZsKC_Type();
  LZsKC_Type = *pLZsKC_Type;
  
	numZ = Z_Total;					// retrieve the globally stored count of Zones
	
	for (iZ = 0; iZ < numZ; iZ++)	// for each KC Zone, ...
	{
		KCZType = LZsKC_Type[iZ];	// retrieve the globally stored Zone type (KC)
			
		for (iN = 0; iN < 8; iN++)			// retrieve the globally stored, 
			ZsNodeID[iN] = LZsLNs[iZ][iN];	// ORDERED, local Node IDs for this zone
		
		EdgesInZone = EdgesPerZone[KCZType];
		for (iE = 0; iE < EdgesInZone; iE++)
		{
			EsN1 = ZsNodeID[ ByZoneTypeEdgeNodes[KCZType][iE][0] ];	// NodeIDs
			EsN2 = ZsNodeID[ ByZoneTypeEdgeNodes[KCZType][iE][1] ];
			
			iLow   = ( EsN1 < EsN2 ) ? EsN1 : EsN2;
			iHigh  = ( EsN1 > EsN2 ) ? EsN1 : EsN2;
						
			Create_Edge = NO;
			pN_Low  = pNode (iLow );		// get pointer to the Node with nNodeLID = iLow
			pN_High = pNode (iHigh);
			pLEI = pN_Low->p1stEdgeLI;		// get the Edge list pointer @ LOW Node
			
			if ( pLEI == NULL)	// Node is storing no Edges yet
			{
				pLEI = MakeList (umEDGE);	// create an Edge list
				pN_Low->p1stEdgeLI = pLEI;	// attach it to the Node
				Create_Edge = YES;
			}
			else //---------- Node already has an Edge list; search for this Edge
			{	if (getIndexInEdgeListWithSameNode2 (pLEI, pN_High) < 0)	// i.e., not found
					Create_Edge = YES;										// => needs creating
			}
			
			if ( Create_Edge == YES )
			{
				pE = allocateEdge (numE);		// create the Edge
				numE++;						// increment count created Edges
				pE->pNode1 = pN_Low;		// point the Edges first Node to the LOW Node
				pE->pNode2 = pN_High;		// point the Edges second Node to the HIGH Node
				
				AddEntityToListEnd (pLEI, umEDGE, pE);	// add new Edge to the list
				
				//----- now add this new Edge to the Edge List at the HIGH Node
				
				pLEI = pN_High->p1stEdgeLI;		// get the Edge list pointer @ HIGH Node
				
				if ( pLEI == NULL)	// Node is storing no Edges yet
				{
					pLEI = MakeList (umEDGE);	// create an Edge list
					pN_High->p1stEdgeLI = pLEI;	// attach it to the Node
				}
				AddEntityToListEnd (pLEI, umEDGE, pE);	// add new Edge to the list
			}
		}
		
	}
/* 	printf("CreateRealEdges:\n    Created %3d Owned Edges.\n", numE); */
  
  setNumE(numE); /* set global number of edges to exactly the number we created. */
  
	return ( numE );
} // End of CreateRealEdges

//======================================================================
void FillNodeIDArray (LItemPtr pNL, int *NIDA, int numN)
{	NodePtr	pN;
	int		nN;
	
	for (nN = 0; nN < numN; nN++)
	{
		pN = RetrieveListItem (pNL, umNODE, nN);	// get a Node in the list
		NIDA [nN] = pN->nNodeLID;					// store its local ID
	}
}

//======================================================================
int IDsAreEqual (int *IDA1, int *IDA2, int numI)
{	int		nI;
	
	for (nI = 0; nI < numI; nI++)
		if (IDA1[nI] != IDA2[nI]) return (NO);

	return (YES);
}

//======================================================================
void Sort_Ints (int *NIDA, int numI)	// using a simple bubblesort
{	int nI, ix;		// for loop indices
	int	tempID;		// to facilitate swap
	
	for (nI = 0; nI < (numI-1) ; nI++)
	{
		for (ix = (nI+1); ix < numI; ix++)
		{
			if (NIDA[ix] < NIDA[nI])
			{
				tempID = NIDA[ix];
				NIDA[ix] = NIDA[nI];
				NIDA[nI] = tempID;
			}
		}
	}
}

//======================================================================
FacePtr FindFaceInFaceList (LItemPtr pLNI,	// list of Nodes of a Face that is looking for its opposite
							LItemPtr pLFI ) // list of Faces to be searched for the opposite
{	
	int		numF, nF;			// # Faces in the list to be checked, and loop index
	int		numN, numN2Test;	// counts of Nodes in two Faces being compared
	int		NodeIDs		  [MAX_FACES_AT_NODE];	// ORDERED array of Node IDs == Face "ID"
	int		NodeIDs2Check [MAX_FACES_AT_NODE];	// ID of Face to be checked
	FacePtr	pF2Test;			// a pointer to a Face on the list to be tested
	
	numN = CountListItems ( pLNI );				// # Nodes in Face that is looking for its opposite
	FillNodeIDArray (pLNI, &NodeIDs[0], numN);	// store the IDs of the Nodes of the Face
	Sort_Ints (&NodeIDs[0], numN);				// sort in preparation to do comparisons
	
	numF = CountListItems ( pLFI );
	for (nF = 0; nF < numF; nF++)
	{
		pF2Test = RetrieveListItem ( pLFI, umFACE, nF);		// get a Face from the list
		numN2Test = CountListItems ( pF2Test->p1stNodeLI );	// count its Nodes
		if (numN2Test == numN)
		{
			FillNodeIDArray (pF2Test->p1stNodeLI, &NodeIDs2Check[0], numN);	// Node IDs of Face to test
			Sort_Ints (&NodeIDs2Check[0], numN);
			if ( IDsAreEqual (&NodeIDs[0], &NodeIDs2Check[0], numN) == YES)
				return ( pF2Test );
		}
	}
	return (NULL);
}

//======================================================================
EdgePtr FindEdgeFromSidesNodes( SidePtr pS )
{
	NodePtr		pN1, pN2, pN_Low;	// pointers to the Side's two Nodes, and "Low" ID Node
	int			nID1, nID2;			// local IDs for the two Nodes
	int			nID_Low, nID_High;	// Low and High local ID numbers
	LItemPtr	pEL;				// pointer to the Edge List at the "Low" Node
	int			numE, nE;			// to loop over Edge list items
	EdgePtr		pE;					// an Edge to test
	
	pN1 = pS->pNode1;				// pointers to the two Nodes
	pN2 = pS->pNode2;
	
	nID1 = pN1->nNodeLID;			// IDs of the two Nodes
	nID2 = pN2->nNodeLID;
	
	nID_Low   = ( nID1 < nID2 ) ? nID1 : nID2;	// get Low and High Node ID numbers
	nID_High  = ( nID1 > nID2 ) ? nID1 : nID2;
	
	pN_Low = pNode ( nID_Low );					// pointer to the Node with the low ID
	pEL    = pN_Low->p1stEdgeLI;				// get its Edge List
	
	if (pEL == NULL)
	{	printf("ERROR No Edges found at Node in FindEdgeFromSidesNodes.\n");
		exit (1);
	}
	
	numE = CountListItems (pEL);
	for (nE = 0; nE < numE; nE++)
	{
		pE = RetrieveListItem ( pEL, umEDGE, nE);		// get a Face from the list
		if (pE->pNode2->nNodeLID == nID_High)			// found
			return (pE);								// return the Side's Edge
	}
	printf("ERROR Edges NOT found at Node in FindEdgeFromSidesNodes.\n");
	exit (1);
}

//======================================================================
void AddSideToNodesSideList (SidePtr pS, NodePtr pN)
{	LItemPtr	pSL;
	
	pSL = pN->p1stSideLI;
	if (pSL == NULL) pSL = MakeList (umSIDE);	// create a Side list
	AddEntityToListEnd (pSL, umSIDE, pS);		// add new Side to the list
	pN->p1stSideLI = pSL;						// attach the list to the node
}


//======================================================================
int MakeSidesOnFace (FacePtr pF, ZonePtr pZ, int numN, int *FsNodeID, int numS)	// NOTE! # of Sides also == numN
{
	SidePtr		pSides[MAX_SIDES_ON_FACE+1];	// to facilitate wrap around for LEFT and RIGHT
	SidePtr		pS, pOS;					// pointer to Side and its opposite Side
	NodePtr		pNodes[MAX_SIDES_ON_FACE+1];	// to facilitate wrap around for 1st and last Side
	int			nN, nS;						// loop indices
	EdgePtr		pE;							// pointer to a Side's Edge
	FacePtr		pOF;						// pointer to the Face's opposite Face
  
	for (nN = 0; nN < numN; nN++)
		pNodes[nN] = pNode(FsNodeID[nN]);	// store pointers to the Face's CCW ORDERED Nodes
	pNodes[numN] = pNodes[0];				// to handle wrap around

	for (nS = 0; nS < numN; nS++)
	{
		pSides[nS] = allocateSide ( numS++ );		// create a Side
		pS = pSides[nS];			// for clearer notation in what follows
		
		pS->pZone = pZ;				// point to the Zone
		pS->pZsNextS = pZ->p1stSide;// link to the front of the Zone's UNORDERED Side list,
		pZ->p1stSide = pS;			// and become the Zone's new 1st Side
		
		pS->pFace = pF;				// point to the Face
		pS->pNode1 = pNodes[nS];	// point to Node 1, on the right
		pS->pNode2 = pNodes[nS+1];	// point to Node 2, on the left. uses wrap around pNodes
		AddSideToNodesSideList ( pS, pS->pNode1);
		AddSideToNodesSideList ( pS, pS->pNode2);
		
		pE = FindEdgeFromSidesNodes( pS );
		pS->pEdge = pE;				// point to the Edge
		pS->pEsNextS = pE->p1stSide;// link to the front of the Edge's UNORDERED Side list,
		pE->p1stSide = pS;			// and become the Edge's 1st Side
	}
	pSides[numN] = pSides[0];				// to handle wrap around
	
	for (nS = 0; nS < numN; nS++)
		pSides[nS]->pLeftSide = pSides[nS+1];	// set LEFT pointers

	for (nS = numN; nS > 0; nS--)
		pSides[nS]->pRightSide = pSides[nS-1];	// set RIGHT pointers

	for (nS = 0; nS < numN-1; nS++)
		pSides[nS]->pFsNextS = pSides[nS+1];	// set Face's next CCW ORDERED Side pointer
	pSides[numN-1]->pFsNextS = NULL;			// terminate the list
	pF->p1stSide = pSides[0];					// attach the list to the Face
	
	pOF = pF->pOppositeFace;
	if (pOF != NULL)	//----- Opposite Face exists, so opposite Sides also exist
	{	pS  = pF->p1stSide;		// 1st Side on Face
		pOS = pOF->p1stSide;	// 1st Side on opposite Face
		
		while (pOS->pNode1 != pS->pNode2)
			pOS = pOS->pLeftSide;	// to "align" pS and pOS; DANGER of infinite loop
		
		pS->pOppositeSide = pOS;	pOS->pOppositeSide = pS;	// point them to each other
		pS = pS->pLeftSide;			pOS = pOS->pRightSide;		// "rotate" in synch
		
		while (pS != pF->p1stSide)	//----- while we haven't arrived back at the start
		{
			pS->pOppositeSide = pOS;	pOS->pOppositeSide = pS;// point them to each other
			pS = pS->pLeftSide;			pOS = pOS->pRightSide;	// "rotate" in synch again
		}
	}

	return (numN);		// the number of Sides created
}

//======================================================================
void LinkAllSideTopPointers ( )
{	EdgePtr		pE;				// to loop over all Edges in the Mesh
	SidePtr		pS;				// to loop over Sides of an Edge, and later, ALL Sides in the Mesh
	int			numS, nS, nS2;	// # of Sides at Edge, loop index, 2nd loop index
	SidePtr		pSides[MAX_SIDES_AT_EDGE];
	ZonePtr		pZ, pZ2;		// Zone numbers for two Sides being tested
	int			nS_withT, nS_withoutT;
	
	pE = pEdge ( 0 );	// get pointer to 1st Edge
	while (pE != NULL)	//-------------------- loop over ALL Edges in the Mesh
	{					//					   could also have used a for loop
		pS = pE->p1stSide;			// 1st Side at this Edge
		pSides[0] = pS;	numS = 1 ;	// store the Side, start the counter
		
		pS = pS->pEsNextS;			// go to the Edge's next Side
		while (pS != NULL)			// until there are no more Sides at this Edge
		{	pSides[numS] = pS;		// store the Side
			numS++;					// increment the count
			pS = pS->pEsNextS;		// go to the Edge's next Side
		}
		for (nS = 0; nS < numS-1; nS++)
		{
			if (pSides[nS]->pTopSide != NULL) continue;	// this matched with an earlier Side
														// no need to search
			pZ = pSides[nS]->pZone;						// Zone pointer to search for
			for (nS2 = nS+1; nS2 < numS; nS2++)			// scan the remaining Sides
			{
				pZ2 = pSides[nS2]->pZone;				// Zone pointer to test with
				if (pZ == pZ2)							// both Sides have the same Zone
				{										// => they are TOP Sides to each other
					pSides[nS ]->pTopSide = pSides[nS2];// link the TOP pointers
					pSides[nS2]->pTopSide = pSides[nS ];
					break;								// no need to search further
				}
			}
		}
		pE = pE->pNextEdge;		// to move to the next Edge in the list of all Edges
	}
	//---------- Now do a check to make sure that all Sides have non-NULL TOP pointers
	nS_withT    = 0;
	nS_withoutT = 0;
	pS = pSide ( 0 );	// get pointer to 1st Side
	while (pS != NULL)
	{
		if (pS->pTopSide == NULL)
			nS_withoutT++;
		else
			nS_withT++;

		pS = pS->pNextSide;		// go to the next Side
	}
	if ( nS_withoutT > 0 )
		{	printf("ERROR Not all Sides have TOP pointers as they should.\n");
			printf("# Sides with = %d, # Sides without = %d.\n", nS_withT, nS_withoutT);
			exit (1);
		}
}

//======================================================================
int CreateRealFacesAndSides ( )
{
	int			numZ, iZ;			// to loop over ALL Zones
	int			numF = 0, numS = 0;	// to count created Faces and Sides
	int			FacesInZone, nF;	// to loop over the Faces of a Zone
	int			NodesInFace, nN;	// to loop over the Nodes of a Zone or Face
	ZonePtr		pZ;					// pointer to the Zone for which Faces are to be created
	int			KCZType;			// the KC type
	LItemPtr	pLNI, pLFI;			// pointer to Node and Face list items
	FacePtr		pF, pOppositeF;		// pointer to the created Face, and its opposite, if it exists
	int			ZsNodeID[8];		// Zone's ORDERED NodeIDs
	int			FsNodeID[4];		// Face's CCW ORDERED NodeIDs wrt Zone interior
	int			nNID,NIDLow;		// a Node's ID, lowest Node ID
	int			numN;				// the number of Nodes in the mesh
	int			numOFaces = 0;		// count opposite Faces found
	NodePtr		pN, pN_Low;			// 
	int			numExtF = 0;		// to count exterior Faces
  int     *LZsKC_Type;
  int    **LZsLNs,**pLZsKC_Type;
  int   ***pLZsLNs;
/*-------------- DEBUG ---------------------*/
#if 0
  int boundaryFaces,ii,jj;
  int *boundaryFaceList;
  int *nodeIds;
#endif
/*-------------- DEBUG ---------------------*/

  pLZsKC_Type = getLZsKC_Type();
  LZsKC_Type = *pLZsKC_Type;
  pLZsLNs = getLZsLNs();
  LZsLNs = *pLZsLNs;
  
  numZ = Z_Total;					// retrieve the globally stored count of Zones
	numN = getNumN ( );				// ask KC how many Nodes there are

	for (iZ = 0; iZ < numZ; iZ++)	// for each KC Zone, ...
	{	
		pZ = pZone ( iZ );			// get a pointer to the Zone
		KCZType = LZsKC_Type[iZ];	// retrieve the globally stored Zone type (KC)

		for (nN = 0; nN < 8; nN++)			// retrieve the globally stored, 
			ZsNodeID[nN] = LZsLNs[iZ][nN];	// ORDERED, local Node IDs for this zone
		
		FacesInZone = FacesPerZone[KCZType];
		for (nF = 0; nF < FacesInZone; nF++)
		{	
			pF = allocateFace ( numF ); numF++;		// create Face, increment count
			pF->pZone    = pZ;					// store the Face's Zone pointer
			pF->pZsNextF = pZ->p1stFace;		// link to the Zone's UNORDERED Face list
			pZ->p1stFace = pF;					//	and become the Zone's 1st Face
      
		//------------------------------------------------- CREATE THE FACE'S CCW ORDERED NODE LIST
			NodesInFace  = ByZoneTypeByFaceNumOfNodes[KCZType][nF];
			pLNI   = MakeList (umNODE);			// create Node list for the Face
			NIDLow = numN + 2;					// initialize > than # of Nodes in the mesh
			
			for (nN = 0; nN < NodesInFace; nN++)
			{
        nNID = ZsNodeID[ ByZoneTypeFaceNodes[KCZType][nF][nN] ];	// NodeID
				FsNodeID[nN] = nNID;										// save it for Side creation
				pN = pNode ( nNID );										// pointer to the Node
				AddEntityToListEnd (pLNI, umNODE, pN);						// add Node to the list
				if (nNID < NIDLow )
					NIDLow = nNID;			// lowest Node ID
			}
			pF->p1stNodeLI = pLNI;			// attach the CCW ORDERED Node list to the Face
		
		//-------------------------------------------------- LOOK FOR OPPOSITE FACE
			pN_Low = pNode (NIDLow);
			
			pLFI = pN_Low->p1stFaceLI;		// get the pointer to the Face list of the "Low" Node
			if ( pLFI == NULL )				// if it does not yet exist, ...
			{	pLFI   = MakeList (umFACE);	// create a new Face list
				pN_Low->p1stFaceLI = pLFI;	// and attach this empty Face list to the Node
			}
			// if this list of Nodes is the same as another Face in the list
			// of Faces at the Node with the lowest ID, then set opposite face pointers
			pOppositeF = FindFaceInFaceList (pLNI, pLFI );
			if ( pOppositeF != NULL )
			{	numOFaces++;					// increment # of opposite Faces found
				pF->pOppositeFace = pOppositeF;	// point to the opposite Face
				pOppositeF->pOppositeFace = pF;	// point opposite Face to the new Face
			}

		//------------------ add Face to the Facelists of all of its Nodes
			for (nN = 0; nN < NodesInFace; nN++)
			{
				pN = RetrieveListItem (pLNI, umNODE, nN);
				pLFI = pN->p1stFaceLI;			// get the pointer to the Face list of the "Low" Node
				if ( pLFI == NULL )				// if it does not yet exist, ...
				{	pLFI   = MakeList (umFACE);	// create a new Face list
					pN->p1stFaceLI = pLFI;		// and attach this empty Face list to the Node
				}
				AddEntityToListEnd (pLFI, umFACE, pF);	// add the new Face to the Face list of the Node
			}

			//------------------------ create sides, connecting LEFT, OPPOSITE and RIGHT pointers
			numS += MakeSidesOnFace (pF, pZ, NodesInFace, &FsNodeID[0], numS);
		}//end of loop over faces in this zone
    
		// I don't know an easy way to connect TOP pointers for all Sides in this Zone at this point
	}//end of loop over zones
  
	LinkAllSideTopPointers ( );		// uses a global loop over Edges

// count external faces and set global IDs
  numExtF=0;
	for (nF= 0; nF < numF; nF++)
	{
		pF = pFace ( nF );
		if (pF->pOppositeFace == NULL)
    {
      numExtF++;
    }
	}
/*-------------- DEBUG ---------------------*/
#if 0
  /* first check:  Are faces planar? */
	LItemPtr	pNLI;							// the Node list of the Face
  int numNodes,numNonPlanarFaces=0;
  for (nF= 0; nF < numF; nF++)                      // loop over all faces in the mesh
	{
		pF = pFace ( nF );
    pNLI = pF->p1stNodeLI;
    numNodes = CountListItems (pNLI);
    double *X = (double*)malloc(numNodes*sizeof(double));
    double *Y = (double*)malloc(numNodes*sizeof(double));
    double *Z = (double*)malloc(numNodes*sizeof(double));
    
    for (nN = 0; nN < numNodes; nN++)
    {
      pN = (NodePtr)RetrieveListItem (pNLI, umNODE, nN);
      X[nN] = pN->Loc3D.x;
      Y[nN] = pN->Loc3D.y;
      Z[nN] = pN->Loc3D.z;
    }

    Loc3DTD normal1, normal2;
    double lenNormal1, lenNormal2;
    normal1.x =    (Y[0]-Y[1])*(Z[2]-Z[1])-(Y[2]-Y[1])*(Z[0]-Z[1]);
    normal1.y = -( (X[0]-X[1])*(Z[2]-Z[1])-(X[2]-X[1])*(Z[0]-Z[1]) );
    normal1.z =    (X[0]-X[1])*(Y[2]-Y[1])-(X[2]-X[1])*(Y[0]-Y[1]);

    lenNormal1 = sqrt(pow(normal1.x,2.0)+pow(normal1.y,2.0)+pow(normal1.z,2.0));
    
    normal2.x =    (Y[2]-Y[3])*(Z[0]-Z[3])-(Y[0]-Y[3])*(Z[2]-Z[3]);
    normal2.y = -( (X[2]-X[3])*(Z[0]-Z[3])-(X[0]-X[3])*(Z[2]-Z[3]) );
    normal2.z =    (X[2]-X[3])*(Y[0]-Y[3])-(X[0]-X[3])*(Y[2]-Y[3]);

    lenNormal2 = sqrt(pow(normal2.x,2.0)+pow(normal2.y,2.0)+pow(normal2.z,2.0));
    
    double dotProd = (normal1.x*normal2.x + normal1.y*normal2.y + normal1.z*normal2.z)/
      (lenNormal1 * lenNormal2);
    double errorInNormals = fabs(1.0 - fabs(dotProd));
    
    if( (errorInNormals > 1.0e-7) || (lenNormal1<1.0e-10) || (lenNormal2<1.0e-10) )
    {
      numNonPlanarFaces++;
      printf(" Face %d is not planar, dot product = %f, fabs(1.0 - fabs(dotProd)) = %e\n",pF->nFaceLID,dotProd,errorInNormals);
      printf(" normal1=(%f,%f,%f), normal2=(%f,%f,%f)\n",normal1.x,normal1.y,normal1.z,normal2.x,normal2.y,normal2.z);
      printf("   nodes: ");
      for (nN = 0; nN < numNodes; nN++)
      {
        pN = (NodePtr)RetrieveListItem (pNLI, umNODE, nN);
        printf("GID=%d, LID=%d (%5.3f,%5.3f,%5.3f) ",pN->nNodeGID,pN->nNodeLID,X[nN],Y[nN],Z[nN]);
      }
      printf("\n");
    }
    else
    {
      printf(" Face %d IS planar, len(normal1)=%e, len(normal2)=%e, dotProd = %e, fabs(acos(angle between normals)) = %e\n",pF->nFaceLID,lenNormal1, lenNormal2, dotProd, errorInNormals);
    }
    
    free(Z);
    free(Y);
    free(X);
  }
  if(numNonPlanarFaces==0)
  {
    printf("NO NON-PLANAR FACES FOUND.\n");
  }

  LItemPtr pLMFI = MakeList (umFACE);	              // create a new Face list for the entire mesh
  for (nF= 0; nF < numF; nF++)                      // loop over all faces in the mesh
	{
		pF = pFace ( nF );
    AddEntityToListEnd (pLMFI, umFACE, pF);	        // add the new Face to the Face list
  }
  
// count external faces and set global IDs
  LItemPtr pLBFI = MakeList(umFACE);                // list of c2k boundary faces
  numExtF=0;
  printf("C2K boundary face IDs:\n");
	for (nF= 0; nF < numF; nF++)
	{
		pF = pFace ( nF );
		if (pF->pOppositeFace == NULL)
    {
      numExtF++;
      AddEntityToListEnd( pLBFI, umFACE, pF );
      printf("%d\n",pF->nFaceLID);
    }
	}
  printf("\n\n");

  cnumbdf(&boundaryFaces);

  printf("Number of Boundary Faces are: %d\n",boundaryFaces);

  boundaryFaceList = (int*)malloc(sizeof(int) * boundaryFaces);

  printf("CMG boundary face IDs:\n");
  cbdrfc(boundaryFaceList);
  LItemPtr pLcmgBFI = MakeList(umFACE);                        // list of cmg's boundary faces
  nodeIds = (int*)malloc(sizeof(int) * 4);
  for(jj=0;jj<boundaryFaces;++jj){
    cfcnda(boundaryFaceList[jj],nodeIds);
    pLNI   = MakeList (umNODE);			                           // create Node list for this Face
    for(ii = 0;ii<4;++ii){
      pN = pNode( nodeIds[ii] );                               // pointer to node on this face
      AddEntityToListEnd (pLNI, umNODE, pN);	                 // add the new Node to the Node list
    }
    FacePtr pCMGF = FindFaceInFaceList (pLNI, pLMFI );         // find the CMG boundary face in the c2k mesh face list
    DeleteList(pLNI);
    if( pCMGF == NULL )
    {
      printf(" face id %d not found!.  Exiting.\n",jj);
      exit(1);
    }else{
      AddEntityToListEnd(pLcmgBFI, umFACE, pCMGF);
      printf("%d\n",pCMGF->nFaceLID);
    }
  }
  free(nodeIds);
  
  printf("\n\n");
  printf("CMG boundary face list size =%d\n",CountListItems(pLcmgBFI));
  printf("C2k boundary face list size =%d\n",CountListItems(pLBFI));

  SetupSeqListAccess( pLBFI, umFACE);  // prepare to loop over the C2K boundary faces

  FacePtr pFaceToCheck = (FacePtr)GetNextEntityFromList( pLBFI, umFACE );
  while( pFaceToCheck != NULL)
  {
    int faceToCheckIndex = FindEntityInList (pLcmgBFI, umFACE, pFaceToCheck );// find the C2K boundary face in the CMG boundary face list
    if(faceToCheckIndex == -1)
    {
      printf("FACE ID=%d from C2K boundary face list NOT FOUND in CMG boundary face list.\n",pFaceToCheck->nFaceLID);
      pNLI = pFaceToCheck->p1stNodeLI;
      numNodes = CountListItems (pNLI);
      printf("  Faces nodes: ");
      for (nN = 0; nN < numNodes; nN++)
      {
        pN = (NodePtr)RetrieveListItem (pNLI, umNODE, nN);
        printf("  %d (%5.3f,%5.3f,%5.3f) ",pN->nNodeLID,pN->Loc3D.x,pN->Loc3D.y,pN->Loc3D.z);
      }
      printf("\n");
    }    
    pFaceToCheck = (FacePtr)GetNextEntityFromList( pLBFI, umFACE );
  }

  /* compare the boundary face lists the other way. */
  printf("\n\n");
  SetupSeqListAccess( pLcmgBFI, umFACE);  // prepare to loop over the CMG boundary faces

  pFaceToCheck = (FacePtr)GetNextEntityFromList( pLcmgBFI, umFACE );
  while( pFaceToCheck != NULL)
  {
    int faceToCheckIndex = FindEntityInList (pLBFI, umFACE, pFaceToCheck );// find the CMG boundary face in the C2K boundary face list
    if(faceToCheckIndex == -1)
    {
      printf("FACE ID=%d from CMG boundary face list NOT FOUND in C2K boundary face list.\n",pFaceToCheck->nFaceLID);
      pNLI = pFaceToCheck->p1stNodeLI;
      numNodes = CountListItems (pNLI);
      printf("  Faces nodes: ");
      for (nN = 0; nN < numNodes; nN++)
      {
        pN = (NodePtr)RetrieveListItem (pNLI, umNODE, nN);
        printf("  %d (%5.3f,%5.3f,%5.3f) ",pN->nNodeLID,pN->Loc3D.x,pN->Loc3D.y,pN->Loc3D.z);
      }
      printf("\n");
    }
    
    pFaceToCheck = (FacePtr)GetNextEntityFromList( pLcmgBFI, umFACE );
  }
  free(boundaryFaceList);
  
#endif
/*-------------- DEBUG ---------------------*/
  
/* 	printf("CreateRealFaces:\n"); */
/*   printf("    Mesh has %3d Owned Zones.\n",numZ); */
/*   printf("    Mesh has %3d Owned Nodes.\n",numN);   */
/* 	printf("    Created %3d Owned Faces.\n", numF); */
/* 	printf("    Created %3d Owned Sides.\n", numS); */
/* 	printf("    Created %3d Owned opposite Face pairs.\n", numOFaces); */
/* 	printf("            %3d Owned Faces are on the boundary.\n", numExtF); */
	
	if ((2*numOFaces + numExtF) != numF)
	{	printf("ERROR #Exterior Faces + #Interior Faces does NOT equal total Faces.\n");
		exit (1);
	}
  
	return (numF);
} // End of CreateRealFacesAndSides

//======================================================================
CornerPtr GetCornerAtNodeWithZone (NodePtr pN, ZonePtr pZ)
{	CornerPtr	pC;
	
	pC = pN->p1stCorner;
	if (pC == NULL)
	{	printf("ERROR Corner NOT found at Node.\n");
		exit (1);
	}
	
	while (pC != NULL)
	{
		if (pC->pZone == pZ) return (pC);
		
		pC = pC->pNsNextC;
	}
	printf("ERROR Corner NOT found at Node.\n");
	exit (1);
}

//======================================================================
void LinkAZonesSidesAndCorners ( ZonePtr pZ )
{	
	SidePtr		pS;		// to loop over Sides of the Zone
	NodePtr		pN;		// temp Node pointer
	CornerPtr	pC;		// temp Corner pointer
	LItemPtr	pSL;	// pointer to Side list

	pS = pZ->p1stSide;
	while (pS != NULL)
	{	
		pN = pS->pNode1;	//-------------------- get the Side's Node 1 (right Node)
		pC = GetCornerAtNodeWithZone ( pN, pZ);	// get the Corner at this Node
		pS->pCorner1 = pC;						// link Side to this Corner
		pSL = pC->p1stSideLI;					// get pointer to the Side list for this Corner
		
		if (pSL == NULL)				// Corner is storing no Sides yet
		{	pSL = MakeList (umSIDE);	// create a Side list
			pC->p1stSideLI = pSL;		// attach the list to the Corner
		}
		AddEntityToListEnd (pSL, umSIDE, pS);	// add the Side to the Corner's Side list
		
		pN = pS->pNode2;	///-------------------- get the Side's Node 2 (left Node)
		pC = GetCornerAtNodeWithZone ( pN, pZ);	// get the Corner at this Node
		pS->pCorner2 = pC;						// link Side to this Corner
		pSL = pC->p1stSideLI;					// get pointer to the Side list for this Corner
		
		if (pSL == NULL)				// Corner is storing no Sides yet
		{	pSL = MakeList (umSIDE);	// create a Side list
			pC->p1stSideLI = pSL;		// attach the list to the Corner
		}
		AddEntityToListEnd (pSL, umSIDE, pS);	// add the Side to the Corner's Side list
		
		pS = pS->pZsNextS;
	} // end of the loop over all Sides of the Zone
}

//======================================================================
void LinkAZonesFacesAndCorners ( ZonePtr pZ )
//-------------------- Now link (in CCW ORDER) the Corners on each Face of the Zone
{	
	FacePtr		pF;				// to loop over a Zone's Faces
	LItemPtr	pCL, pFL;		// pointer to Corner and Face list items
	SidePtr		pS;				// to loop over a Face's Sides
	CornerPtr	pC;				// temp Corner pointer
	
	pF = pZ->p1stFace;
	while (pF != NULL)
	{	
		pCL = MakeList (umCORNER);	// create a new Corner list for the Face
		pF->p1stCornerLI = pCL;		// attach it to the Face
		
		pS = pF->p1stSide;
		while (pS != NULL)			// loop over all Sides of the Face
		{
			pC = pS->pCorner1;
			AddEntityToListEnd (pCL, umCORNER, pC);	// add to the Face's Corner list
			
			pFL = pC->p1stFaceLI;
			if (pFL == NULL)					// if the Face list does not yet exist, ...
			{
				pFL = MakeList (umFACE);		// create the Face list
				pC->p1stFaceLI = pFL;			// attach the list to the Corner
			}
			AddEntityToListEnd (pFL, umFACE, pF);	// add the Face to the Corner's Face list
			
			pS = pS->pFsNextS;						// go to Face's next Side
		}
		pF = pF->pZsNextF;							// go to Zone's next Face
	} // end of loop over all Faces of the Zone
}


//======================================================================
int CreateRealCorners ( )
{
	ZonePtr		pZ;				// to loop over Zones
	int			iZ, numZ;		// Zone loop index, # of Zones
	int			NodesInZone;	// the number of Nodes (and Corners) in a Zone
	int			numC = 0;		// the number of Corners created
//	int			KCZType;		// KC Zone type
//	int			ZsNodeID[8];	// Zone's ORDERED NodeIDs
	int			ZsCornerID[8];	// Zone's CornerIDs in the same order as the Nodes
	CornerPtr	pC;				// pointer to a Corner
	NodePtr		pN;				// pointer to a Node
	int			nN;				// loop index over Nodes
	LItemPtr	pZsNodeList;	// to collect a Zone's Nodes
	SidePtr		pS;				// to loop over a Zone's Sides
	
	numZ = getNumZ ( );			// get the # of Zones in the mesh
	for (iZ = 0; iZ < numZ; iZ++)
	{
		pZ = pZone ( iZ );					// get a pointer to the Zone
		
		NodesInZone = 0;
		pZsNodeList = MakeList (umNODE);
		pS			= pZ->p1stSide;
		
		while (pS != NULL)
		{
			if (FindEntityInList   (pZsNodeList, umNODE, pS->pNode1) < 0)	// i.e., NOT found
			{	AddEntityToListEnd (pZsNodeList, umNODE, pS->pNode1);		// store the Node pointer
				NodesInZone++;												// increment Node count
			}
			
			if (FindEntityInList   (pZsNodeList, umNODE, pS->pNode2) < 0)	// i.e., NOT found
			{	AddEntityToListEnd (pZsNodeList, umNODE, pS->pNode2);		// store the Node pointer
				NodesInZone++;												// increment Node count
			}
			
			pS = pS->pZsNextS;
		}
		
		for (nN = 0; nN < NodesInZone; nN++)	// loop over the Nodes of the Zone
		{
			pN = (NodePtr)RetrieveListItem(pZsNodeList, umNODE, nN);
			
			pC = allocateCorner ( numC );			// create a Corner at this Node
			numC++;								// increment the Corner count
			pC->pNode = pN;						// pointer to the Corner's Node
			pC->pZone = pZ;						// pointer to the Corner's Zone
			
			pC->pZsNextC = pZ->p1stCorner;		// put Corner at the front of the Zone's Corner list
			pZ->p1stCorner = pC;				// attach the lengthened Corner list to the Zone
			
			pC->pNsNextC = pN->p1stCorner;		// put Corner at the front of the Node's Corner list
			pN->p1stCorner = pC;				// attach the lengthened Corner list to the Node
			
			ZsCornerID[nN] = pC->nCornerLID;		// temporarily store the CornerID ????? NEEDED ?????
		} // end of loop over the Nodes of the Zone
		
		DeleteList (pZsNodeList);
		
		LinkAZonesSidesAndCorners ( pZ );	// loop over a a Zone's Sides, find Corners, mutually link
				
		LinkAZonesFacesAndCorners ( pZ );	// loop over a Zone's Faces,Sides, get Corners, mutually link
		
	}  // end of the Zone loop
	
/* 	printf("CreateRealCorners:\n    Created %3d Owned Corners.\n", numC); */
	
	return ( numC );
} // End of CreateRealCorners


//======================================================================
int SetZoneSDControl (ZonePtr pZ )
{
	FacePtr		pF;	// to loop over Faces of a Zone

	pF = pZ->p1stFace;
	while (pF != NULL)
	{
		if (pF->pOppositeFace == NULL)
			return ( 0 );	// any boundary Face keeps the Zone's sub-division off
		
		pF = pF->pZsNextF;
	}
	pZ->SubDivisionControl = SUBDIVISION_OK;
	return ( 1 );
}

void SetZonesSDControl ( )
{
	int		numZ, iZ;	// to loop over Zones
	ZonePtr	pZ;			// Zone to be examined
	int		numSDZ = 0;	// # of Zones for which sub-division is OK
	
	numZ = getNumZ ( );				// get the # of Zones in the mesh
	for (iZ = 0; iZ < numZ; iZ++)
	{
		pZ = pZone ( iZ );					// get a pointer to the Zone
		
		numSDZ += SetZoneSDControl (pZ);	// set control and update count
	}
	
/* 	printf("SetZonesSDControl:\n            %3d Zone(s) can be subdivided.\n", numSDZ); */
}

//======================================================================
void IdentifyAndCountBoundaryFaces ( )
{
	int		numF, iF;	// to loop over (Zone) Faces
	FacePtr	pF;			// Face to be processed
	int i, j, idSize, *ids, numMeshTags = -1;
  int tagNameIndexCount;
  int myRank;
	int			GBFs_GNID [4];	// the global IDs of the four Nodes of a GLOBAL boundary Face
	int			DBFs_GNID [4];	// the global IDs of the Nodes of a DOMAIN boundary Face
  int iDBF,iN;
  NodePtr pN;
  char** theFaceTags;
  int tagType;
  int *DBF_LID, *DBF_GID;
  int **pDBF_LID, **pDBF_GID;

  pDBF_LID = getDBF_LID();
  DBF_LID = *pDBF_LID;
  pDBF_GID = getDBF_GID();
  DBF_GID = *pDBF_GID;
  
	DBF_Total = 0;					// initialize the counter of domain boundary Faces
	numF = getNumF ( );				// get the # of Zonal Faces in the mesh
	for (iF = 0; iF < numF; iF++)	// loop over Faces
	{
		pF = pFace ( iF );
		if (pF->pOppositeFace == NULL)	// No opposite Face, means the Face is on a DOMAIN boundary
		{
			DBF_LID [DBF_Total] = pF->nFaceLID;
			DBF_GID [DBF_Total] = -1;			// will be OVERRIDDEN if this is a communication Face
			DBF_Total++;
			
			pF->FType = PROBLEM_BOUNDARY_FACE;	// This will be OVERRIDDEN if this a communication Face.
		}										// Any Faces NOT changed are true boundaries
		else
			pF->FType = INTERIOR_FACE;
	}

  // add boundary tag information.  We first determine all possible boundary
  // face tag names and put them into the DBF_Tags array.  Then loop over all
  // tags and set the tag index for each boundary face.

  myRank=0;
#ifdef USE_MPI
  MPI_Comm_rank(MPI_COMM_WORLD,&myRank);
  
#endif
  cmtgs(&numMeshTags);  // get the total number of mesh tags for this domain
/*   printf("on proc %d, number of mesh tags = %d\n",myRank,numMeshTags); */
  
  if( numMeshTags > MAX_NUM_BOUNDARY_FACE_TAGS)
  {
    printf("ERROR in C2K-KC_Create.c IdentifyAndCountBoundaryFaces, Too many boundary face tags.\n");
  }

  tagNameIndexCount=0;
  
  // set DBF_Tags[0] to a null value and increment counter
  theFaceTags = getBoundaryFaceTags();
  
  theFaceTags[0] = (char*)malloc(sizeof(char)*(strlen("NULL")+1));
  strcpy(theFaceTags[0],"NULL");
  tagNameIndexCount++;
  
  for(i=0;i<numMeshTags;i++)
  {
    cmtgtp(i,&tagType);  // get integer tag type from CMG (0-node, 1-edge, 2-face, 3-zone, 4-material)

    if( tagType==2 ) // only handle face tags
    {  
      char tagName[100];
      cmtgnm(i,tagName);                        // get the tag name from CMG
      theFaceTags[tagNameIndexCount] = (char*)malloc(sizeof(char)*(strlen(tagName)+1));
      strcpy(theFaceTags[tagNameIndexCount],tagName);

      cmtgidsz(i,&idSize);                      // get number of face IDs for this tag
      ids = (int*)malloc(sizeof(int) * idSize); // allocate face id array
      cmtgid(i,ids);                            // get the face ids from CMG

      for( j=0;j<idSize;++j)                // loop over GLOBAL face ids for this tag
      {
        cfcnda (ids[j], &GBFs_GNID[0] );	// get four node GIDs for this tagged GLOBAL boundary face
		
        Sort_Ints ( &GBFs_GNID[0], 4);		// sort the four Node GIDs to create a "Face ID"
		
        for (iDBF = 0; iDBF < DBF_Total; iDBF++)	//-------------------- loop over all domain boundary Faces ...
        {											//					   looking for this COMM boundary Face
          pF = pFace ( DBF_LID[iDBF] );

          if (NumNodesOnFace (pF) != 4) continue;		// REAL CMG only has quadrilateral COMM Faces

          for (iN = 0; iN < 4; iN++)			// loop over the Nodes of the Face
          {
            pN = RetrieveListItem (pF->p1stNodeLI, umNODE, iN);
            DBFs_GNID [iN] = pN->nNodeGID;						// get the Node's global ID
          }
          Sort_Ints ( &DBFs_GNID[0], 4);		// sort the four Node GIDs to create a "Face ID"
			
          if (IDsAreEqual ( &DBFs_GNID[0], &GBFs_GNID[0], 4) == YES)	//********** MANY CONNECTIONS ARE SET HERE
          {
            pF->FaceTagIndex = tagNameIndexCount;  // assign this face's tag name index 
									
          }	// end of work on the found COMM boundary Face
        }	// end of loop over domain boundary Faces

      }//end of loop over face ids with this tag
      free(ids);
      tagNameIndexCount++;                      // increment tag name index counter
    }//end of face tag type condition
    
  }//end of loop over mesh tags
  
}

//======================================================================
void CreateBoundaryElements ( )
{
	int			iDBF, iC;	// to loop over COMM and DOMAIN boundary Faces, and Corners
	FacePtr		pF;			// a Face to be processed
	CornerPtr	pC;			// a Corner to be processed
	BElemPtr	pBE;		// to create and initialize a boundary element
	int *DBF_LID;
	int **pDBF_LID;

  pDBF_LID = getDBF_LID();
  DBF_LID = *pDBF_LID;
  
	setNumBE ( 4*DBF_Total );	// this allocates boundary ELEMENT storage

		for (iDBF = 0; iDBF < DBF_Total; iDBF++)	//-------------------- loop over all domain boundary Faces ...
		{											//					   looking for this COMM boundary Face
			pF = pFace ( DBF_LID[iDBF] );	// get Face with the given local ID

			if (NumNodesOnFace (pF) != 4) continue;	// real CMG will only have 4-sided boundary Faces, but FAKE_CMG ...
			
			for (iC = 0; iC < 4; iC++)			// loop over the Corners of the Face
			{
				pC = RetrieveListItem (pF->p1stCornerLI, umCORNER, iC);
				
				pBE = allocateBElem ( );	// allocate the boundary element
				pBE->pCorner = pC;			// point to its Corner (through Corner can get Node)
				pBE->pFace   = pF;			// point to its Face
			}
		}
}

//======================================================================
void IdentifyCommBoundaryFaces ( )
{
	int			iCBF, iDBF, iN;	// to loop over COMM and DOMAIN boundary Faces, and Nodes
	int			CBFs_GID;		// a boundary Face's global ID
	int			CBFs_GNID [4];	// the global IDs of the four Nodes of a COMM boundary Face
	int			DBFs_GNID [4];	// the global IDs of the Nodes of a DOMAIN boundary Face
	NodePtr		pN;				// to loop over the Nodes of a DOMAIN boundary Face
	FacePtr		pF;				// Face to be processed
	int			nCN;			// loop index
	int     numRecvN; // number of receieve neighbors
  int     numSndN;  // number of send neighbors
  int     *CBF_LID,*DBF_GID,*CBF_GID,*DBF_LID,*CN_DomainID,*CN_NumFaces;
  int     **pCBF_LID,**pDBF_GID,**pCBF_GID,**pDBF_LID,**pCN_DomainID,**pCN_NumFaces;

  pCBF_LID = getCBF_LID();
  CBF_LID = *pCBF_LID;
  pDBF_GID = getDBF_GID();
  DBF_GID = *pDBF_GID;
  pCBF_GID = getCBF_GID();
  CBF_GID = *pCBF_GID;
  pDBF_LID = getDBF_LID();
  DBF_LID = *pDBF_LID;  
  pCN_DomainID = getCN_DomainID();
  CN_DomainID = *pCN_DomainID;
  pCN_NumFaces = getCN_NumFaces();
  CN_NumFaces = *pCN_NumFaces;
  
	crecfsz (&numRecvN);		// get the # of RECEIVE neighbors, and store into global variable
	NumCommN = numRecvN;
  
	if (NumCommN > MAX_CN)
	{	printf("ERROR; Too many communication neighbors\n");
		printf("# requested = %d, but MAX_CN = %d\n",NumCommN, MAX_CN);
		exit(1);
	}
	
	crecfar (&CN_DomainID[0], &CN_NumFaces[0]);	// get the lists of domain IDs and # of Faces for each domain
	
	CBF_Total = 0;								// initialize index for CBF_GID storage
	for (nCN = 0; nCN < numRecvN; nCN++)		//----- loop over all COMM neighbors
	{
		crecf ( CN_DomainID[nCN], &CBF_GID[CBF_Total]);	// get the global Face IDs for a COMM neighbor
		
		CBF_Total += CN_NumFaces[nCN];					// increment the total COMM Face count
	}
	
	for (iCBF = 0; iCBF < CBF_Total; iCBF++)	//-------------------- loop over all COMM boundary Faces
	{
		CBFs_GID  = CBF_GID[iCBF];			// COMM boundary Face's global ID
		
		cfcnda (CBFs_GID, &CBFs_GNID[0] );	// get four node GIDs for the COMM boundary face
		
		Sort_Ints ( &CBFs_GNID[0], 4);		// sort the four Node GIDs to create a "Face ID"
		
		for (iDBF = 0; iDBF < DBF_Total; iDBF++)	//-------------------- loop over all domain boundary Faces ...
		{											//					   looking for this COMM boundary Face
			pF = pFace ( DBF_LID[iDBF] );

			if (NumNodesOnFace (pF) != 4) continue;		// REAL CMG only has quadrilateral COMM Faces

			for (iN = 0; iN < 4; iN++)			// loop over the Nodes of the Face
			{
				pN = RetrieveListItem (pF->p1stNodeLI, umNODE, iN);
				DBFs_GNID [iN] = pN->nNodeGID;						// get the Node's global ID
			}
			Sort_Ints ( &DBFs_GNID[0], 4);		// sort the four Node GIDs to create a "Face ID"
			
			if (IDsAreEqual ( &DBFs_GNID[0], &CBFs_GNID[0], 4) == YES)	//********** MANY CONNECTIONS ARE SET HERE
			{
				CBF_LID[iCBF]  = pF->nFaceLID;	// save the local ID of a COMM boundary Face
				DBF_GID[iDBF]  = CBFs_GID;		// save the global ID of a domain boundary Face
				pF->FType      = RECEIVE_FACE;		// OVERRIDE the Face type
				pF->nFaceGID   = CBFs_GID;		// save GID in the Face struct for COMM Faces
									
			}	// end of work on the found COMM boundary Face
		}	// end of loop over domain boundary Faces
	}	// end of loop over RECEIVE boundary Faces

  
	csndfsz (&numSndN);		// get the # of send faces, and store into global variable
	NumCommN = numSndN > numRecvN?numSndN:numRecvN; //set NumCommN to the larger of the number of send and receive domains
  
	if (NumCommN > MAX_CN)
	{	printf("ERROR; Too many communication neighbors\n");
		printf("# requested = %d, but MAX_CN = %d\n",NumCommN, MAX_CN);
		exit(1);
	}
	
	csndfar (&CN_DomainID[0], &CN_NumFaces[0]);	// get the lists of domain IDs and # of Faces for each domain
	
	CBF_Total = 0;								// initialize index for CBF_GID storage
	for (nCN = 0; nCN < numSndN; nCN++)		//----- loop over all COMM neighbors
	{
		csndf ( CN_DomainID[nCN], &CBF_GID[CBF_Total]);	// get the global Face IDs for a COMM neighbor
		
		CBF_Total += CN_NumFaces[nCN];					// increment the total COMM Face count
	}
	
	for (iCBF = 0; iCBF < CBF_Total; iCBF++)	//-------------------- loop over all COMM boundary Faces
	{
		CBFs_GID  = CBF_GID[iCBF];			// COMM boundary Face's global ID
		
		cfcnda (CBFs_GID, &CBFs_GNID[0] );	// get four node GIDs for the COMM boundary face
		
		Sort_Ints ( &CBFs_GNID[0], 4);		// sort the four Node GIDs to create a "Face ID"
		
		for (iDBF = 0; iDBF < DBF_Total; iDBF++)	//-------------------- loop over all domain boundary Faces ...
		{											//					   looking for this COMM boundary Face
			pF = pFace ( DBF_LID[iDBF] );

			if (NumNodesOnFace (pF) != 4) continue;		// REAL CMG only has quadrilateral COMM Faces

			for (iN = 0; iN < 4; iN++)			// loop over the Nodes of the Face
			{
				pN = RetrieveListItem (pF->p1stNodeLI, umNODE, iN);
				DBFs_GNID [iN] = pN->nNodeGID;						// get the Node's global ID
			}
			Sort_Ints ( &DBFs_GNID[0], 4);		// sort the four Node GIDs to create a "Face ID"
			
			if (IDsAreEqual ( &DBFs_GNID[0], &CBFs_GNID[0], 4) == YES)	//********** MANY CONNECTIONS ARE SET HERE
			{
				CBF_LID[iCBF]  = pF->nFaceLID;	// save the local ID of a COMM boundary Face
				DBF_GID[iDBF]  = CBFs_GID;		// save the global ID of a domain boundary Face
				pF->FType      = SEND_FACE;		// OVERRIDE the Face type
				pF->nFaceGID   = CBFs_GID;		// save GID in the Face struct for COMM Faces
									
			}	// end of work on the found COMM boundary Face
		}	// end of loop over domain boundary Faces
	}	// end of loop over SEND boundary Faces
}

//======================================================================
void SetFacesBoundaryState ( )
{	
	IdentifyAndCountBoundaryFaces ( );

	CreateBoundaryElements ( );
	
	IdentifyCommBoundaryFaces ( );
}

//======================================================================
int CreateCommunicationData ( )
{
  int* nRecvNbrs = getNumberOfReceiveNeighbors();/* a single int defining how many neigbors I receive from */
  int* rcvNbrIds = getReceiveNeighborIDs();      /* dimensioned [MAX_NUM_NEIGHBORS] */
  int* rcvNumFaces = getNumReceiveFaces();       /* dimensioned [MAX_NUM_NEIGHBORS][MAX_NODES] */
  int* rcvFaceGIDs;
  FacePtr* recvFaces;
    
  int* nSendNbrs = getNumberOfSendNeighbors(); /* a single int defining how many neigbors I send to */
  int* sendNbrIds = getSendNeighborIDs();      /* dimensioned [MAX_NUM_NEIGHBORS] */
  int* sendNumFaces = getNumSendFaces();       /* dimensioned [MAX_NUM_NEIGHBORS][MAX_NODES] */
  int* sendFaceGIDs;
  FacePtr* sendFaces;
  int* numNodesDomain;
  FacePtr theFacePtr;
  int theFacesLID;
  
  int ix,iy,myProc,numSendFaces,numRecvFaces,theSendNbrID,theRecvNbrID;

  myProc=0;
#ifdef USE_MPI
  MPI_Comm_rank(MPI_COMM_WORLD,&myProc);
#endif
  
  csndfsz( nSendNbrs ); /* set number of neighbors I send to */
  csndfar( sendNbrIds, sendNumFaces); /* set the nbr ids and number of faces I send to */
  
/*   printf("proc %d has %d send neighbors.\n",myProc,*nSendNbrs); */
  
  /* for each neighbor I send to, get the IDs of the faces I send and convert
     them to FacePtrs and store in global data */
  for(ix = 0; ix < *nSendNbrs; ix++)
  {
    theSendNbrID = sendNbrIds[ix];
    
    sendFaces = getSendFaces( theSendNbrID );

    numSendFaces = sendNumFaces[ix];
    
    sendFaceGIDs = (int*)malloc(sizeof(int)*numSendFaces);
    
    csndf(theSendNbrID,sendFaceGIDs ); /* get the Global IDs of the faces I send to process sendNbrIds[ix] */

    /* convert sendFaceIDs to FacePtrs and store in global data */
    for(iy = 0 ;iy < numSendFaces; iy++)
    {
      theFacesLID = -1;
      /* for each send face find it's Local ID */
      for( theFacePtr = Owned_Face_Begin(); theFacePtr != Owned_Face_End(); theFacePtr = Next_Owned_Face() )
      {
        if( theFacePtr->nFaceGID == sendFaceGIDs[iy] )
        {
          theFacesLID = theFacePtr->nFaceLID;
          break;
        }
      }
      assert( theFacesLID  != -1 );
      
      sendFaces[iy] = pFace(theFacesLID);
/*       printf("proc %d has a send face GID: %d, LID:%d\n",myProc,sendFaceGIDs[iy],theFacesLID); */
    }
    
    free( sendFaceGIDs );
    
  }/* end of loop over send neighbors */


  crecfsz( nRecvNbrs ); /* set number of neighbors I receive from */
  crecfar( rcvNbrIds , rcvNumFaces);/* set the nbr ids and number of faces I receive from */
  
/*   printf("proc %d has %d receive neighbors.\n",myProc,*nRecvNbrs); */
  
  /* for each neighbor I receive from, get the IDs of the faces I receive and convert
     them to FacePtrs and store in global data */
  for(ix = 0; ix < *nRecvNbrs; ix++)
  {
    theRecvNbrID = rcvNbrIds[ix];
    
    recvFaces = getReceiveFaces( theRecvNbrID );

    numRecvFaces = rcvNumFaces[ix];
    
    rcvFaceGIDs = (int*)malloc(sizeof(int)*numRecvFaces);
    
    crecf(theRecvNbrID,rcvFaceGIDs ); /* get the Global IDs of the faces I receive from process rcvNbrIds[ix] */

    /* convert rcvFaceIDs to FacePtrs and store in global data */
    for(iy = 0 ;iy < numRecvFaces; iy++)
    {
      theFacesLID = -1;
      /* for each send face find it's Local ID */
      for( theFacePtr = Owned_Face_Begin(); theFacePtr != Owned_Face_End(); theFacePtr = Next_Owned_Face() )
      {
        if( theFacePtr->nFaceGID == rcvFaceGIDs[iy] )
        {
          theFacesLID = theFacePtr->nFaceLID;
          break;
        }
      }
      assert( theFacesLID  != -1 );
      recvFaces[iy] = pFace(theFacesLID);
/*       printf("proc %d has a receive face GID: %d, LID:%d\n",myProc,rcvFaceGIDs[iy],theFacesLID); */
    }
    
    free( rcvFaceGIDs );
    
  }/* end of loop over send neighbors */

  return 0;
}

//======================================================================
void Create_KC_Mesh ( )
{
	CreateRealNodes ( );
	CreateRealZones ( );
	CreateRealEdges ( );
	CreateRealFacesAndSides ( );
  CalcAllFaceCentroids();
  
/*   SetZonesSDControl ( ); */
/*   SubDivideZones ( 0.1 ); */
    
	CreateRealCorners ( );
/* 	PrintCountOfAllListItems ( );	// to verify the "Set Sizes" spreadsheet */

  /* print max number of faces per corner. */
/*   printf("MAX number of faces per corner AFTER Subdivision= %d\n",C2K_maxcf() ); */
  
	SetFacesBoundaryState ( );

#ifndef CMG_FAKE
  CreateCommunicationData ( );
#endif
}

//=================================================================================
//			Functions that create data specifically for Teton, in Teton's formats
//			These functions may be called ONLY AFTER Create_KC_Mesh has been called
//=================================================================================

//======================================================================
int C2K_ncornr  ( )			// # of Corners in the mesh
{
	return ( getNumC ( ) );	// simply use the storage access function
}

//======================================================================
int C2K_maxcf   ( )			// Maximum # of Faces found at a Corner
{
	int			maxcf, numcf;	// to hold the maximum # faces found looping over all Corners
	CornerPtr	pC;				// to loop over Corners
	
	maxcf = -1;					// initialize
	pC    = pCorner ( 0 );		// start with the first Corner in the mesh
	
	while ( pC != NULL )
	{
		numcf = CountListItems ( pC->p1stFaceLI );
		if (numcf > maxcf)
			maxcf = numcf;
			
		pC = pC->pNextCorner;	// go to the next Corner in the mesh
	}
	return (maxcf);
}

//======================================================================
int C2K_nbshare ( )		// # shared (i.e., "COMM") boundary elements
{
	int		numConF;	// the number of Corners on a (Zone) Face
	int		iCBF;		// to loop over COMM boundary (Zone) Faces in the mesh
	int		nbshare;	// total # of share boundary elements == # of CFaces on COMM (Zone) Faces
	FacePtr	pF;			// a Face to examine
	int *CBF_LID;
	int **pCBF_LID;

  pCBF_LID = getCBF_LID();
  CBF_LID = *pCBF_LID;
  
	nbshare = 0;		// initialize the counter
	
	for (iCBF = 0; iCBF < CBF_Total; iCBF++)					// loop over COMM Boundary Faces
	{
		pF = pFace ( CBF_LID [iCBF] );							// get a pointer to the Face struct
		if (pF->pOppositeFace == NULL)							// if on the domain boundary
				numConF = CountListItems ( pF->p1stCornerLI );	// get # of Corners on the Face
		nbshare += numConF;										// add to COMM boundary element count
	}
	return (nbshare);
}

//======================================================================
int C2K_nbelem  ( )		// # of boundary elements == boundary CFaces
{
	int		numConF;	// the number of Corners on a (Zone) Face
	int		iDBF;		// to loop over domain boundary (Zone) Faces in the mesh
	int		nbelem;		// total # of boundary elements == # of boundary CFaces
	FacePtr	pF;			// a Face to examine
	int *DBF_LID;
	int **pDBF_LID;

  pDBF_LID = getDBF_LID();
  DBF_LID = *pDBF_LID;
  
	nbelem = 0;			// initialize the counter
	
	for (iDBF = 0; iDBF < DBF_Total; iDBF++)					// loop over Domain Boundary Faces
	{
		pF = pFace ( DBF_LID [iDBF] );							// get a pointer to the Face struct
		if (pF->pOppositeFace == NULL)							// if on the domain boundary
				numConF = CountListItems ( pF->p1stCornerLI );	// get # of Corners on the Face
		if (numConF != 4) continue;								// REAL CMG boundary Faces will all have 4 Corners
		nbelem += numConF;										// add to boundary element count
	}
	return (nbelem);
}

//======================================================================
int C2K_ncomm   ( )		// # of communication neighbor domains
{
	int		numOfDomains;
	
	crecfsz ( &numOfDomains );
	return (numOfDomains);
}

//======================================================================
void C2K_px (double * px)	// the 3D locations of all "points" (i.e., Nodes)
{
	NodePtr		pN;		// to loop over all Nodes in the mesh
	int			i;		// to increment the storage location in the array px
	
	pN = pNode ( 0 );	// start at the first Node
	i  = 0;				// start at the first element of px
	
	while (pN != NULL)
	{
		px[i] = pN->Loc3D.x;	i++;
		px[i] = pN->Loc3D.y;	i++;
		px[i] = pN->Loc3D.z;	i++;
		
		pN = pN->pNextNode;		// go to the next Node
	}
}

//======================================================================
void C2K_nfpc (int    *nfpc)	// # of CFace pairs for each Corner in the mesh
								// same as # of (Zone) Faces per corner
{
	int			iC;		// to increment the storage location in nfpc
	CornerPtr	pC;		// to loop over all Corners
	
	pC = pCorner ( 0 );	// start with the first Corner
	iC = 0;				// start with the first storage location in the array nfpc
	
	while (pC != NULL)
	{
		nfpc[iC] = CountListItems ( pC->p1stFaceLI );
		iC++;
		
		pC = pC->pNextCorner;	// go to the next Corner
	}
}

//======================================================================
void C2K_ctozone (int *ctozone)	// zero origin index of Zone of the selected Corner
{
	int			iC;	// to index into ctozone
	CornerPtr	pC;	// to loop over Corners
	
	pC = pCorner ( 0 );		// start at the first Corner
	iC = 0;					// start at the first storage location in the array in ctozone
	
	while (pC != NULL)
	{
		ctozone[iC] = pC->pZone->nZoneLID + 1;	// the 1 origin LOCAL ID of the Corner's Zone
		iC++;									// increment storage location
		
		pC = pC->pNextCorner;	// go to the next Corner
	}
}

//======================================================================
void C2K_ctopoint(int *ctopoint)	// ZERO ORIGIN index of Node ("point") of the selected Corner
{
	int			iC;	// to index into ctopoint
	CornerPtr	pC;	// to loop over Corners
	
	pC = pCorner ( 0 );		// start at the first Corner
	iC = 0;					// start at the first storage location in the array in ctopoint
	
	while (pC != NULL)
	{
		ctopoint[iC] = pC->pNode->nNodeLID + 1;	// the 1 origin LOCAL ID of the Corner's Node
		iC++;									// increment storage location
		
		pC = pC->pNextCorner;	// go to the next Corner
	}
}

//======================================================================
void C2K_ctoface (int  *ctoface)	// ZERO ORIGIN index of FaceS of the selected Corner
{
	int			maxcfDIM;	// the first index limit in ctoface
	int			iC;			// 2nd index into ctoface
	int			ixF, nF;	// to index over Faces at a Corner
	CornerPtr	pC;			// to loop over Corners
	FacePtr		pF;			// a Face to examine
	
	maxcfDIM = C2K_maxcf( );// get the first array index dimension
	pC = pCorner ( 0 );		// start at the first Corner
	iC = 0;					// start at the first storage location in the array in ctoface
	
	while (pC != NULL)
	{
		nF = CountListItems ( pC->p1stFaceLI );		// the # of (Zone) Faces at this Corner
		
		for (ixF = 0; ixF < nF; ixF++)				// loop of the Faces at the Corner
		{
			pF = (FacePtr)RetrieveListItem (pC->p1stFaceLI, umFACE, ixF);	// get the Face's pointer
			
			ctoface[ ixF + iC ] = pF->nFaceLID + 1;	// the 1 origin LOCAL ID of the Face
		}
				
		iC += maxcfDIM;			// increment the second index of storage location
		
		pC = pC->pNextCorner;	// go to the next Corner
	}
}

//======================================================================
void OrderTwoArraysByFirstArrayContent (int *iA, int *iB, int numI)
{	int nI, ix;	// for loop indices
	int	tempID;	// to facilitate swap
											// a simple "bubble sort"
	for (nI = 0; nI < (numI-1) ; nI++)		// future optimization needed
	{
		for (ix = (nI+1); ix < numI; ix++)
		{
			if (iA[ix] < iA[nI])
			{
				tempID = iA[ix];
				iA[ix] = iA[nI];
				iA[nI] = tempID;
				
				tempID = iB[ix];
				iB[ix] = iB[nI];
				iB[nI] = tempID;
			}
		}
	}
}

//======================================================================
void C2K_nodecomm(int *nodecomm)	// COMM neighbor domain IDs and # boundary elements shared
{
	int		ncomm;					// to store the # of comm neighbor domains
	int	   *DomainIDs;				// Domain IDs of COMM neighbor domains, dynamic array
	int	   *NumBdyFacesOfDomain;	// # of boundary Faces of domain, dynamic array
	int		iD,ix;					// to loop over the domains
	
	ncomm = C2K_ncomm ( );	// get the # of COMM neighbor domains
	
	DomainIDs           = malloc (ncomm*sizeof(int));
	NumBdyFacesOfDomain = malloc (ncomm*sizeof(int));
	
	crecfar ( DomainIDs, NumBdyFacesOfDomain );	// get from CMG an array of domain IDs for COMM neighbors
												// & an array of the # of shared faces with each domain
	
	//---------- Now assure domain ID array is in ascending order
	OrderTwoArraysByFirstArrayContent (DomainIDs, NumBdyFacesOfDomain, ncomm);
	
	ix = 0;
	for ( iD = 0; iD < ncomm; iD++)		// interleave the two arrays into one 2D array for Teton
	{
		nodecomm [ix  ] = DomainIDs[iD];
		nodecomm [ix+1] = NumBdyFacesOfDomain[iD] * 4;	// CMG only communicates via quadrilateral Faces
		ix += 2;
	}
	free(DomainIDs);
	free(NumBdyFacesOfDomain);
}

//======================================================================
int BElemNumFromFaceAndNodeGIDs (int GlobalFaceID, int GlobalNodeID)	// NEEDS OPTIMIZATION
{
	int		NumBE, iBE;	// # of boundary elements, loop index
	BElemPtr	pBE;	// a boundary element to examine
	
	NumBE = getNumBE( );
	
	for (iBE = 0; iBE < NumBE; iBE++)
	{
		pBE = pBElem (iBE);
		if (pBE->pFace->nFaceGID == GlobalFaceID)
		{
			if (pBE->pCorner->pNode->nNodeGID == GlobalNodeID)
				return (iBE);
		}
	}
	printf("ERROR Failed to find Corner from Face and Node GIDs.\n");
	exit (1);
}

//======================================================================
void C2K_cbdycomm(int *cbdycomm)	// LOCAL boundary elements #s in a VERY SPECIAL order
{
	int	   *DomainIDs;				// Domain IDs of COMM neighbor domains,					dynamic array
	int	   *NumBdyFacesOfDomain;	// # of boundary Faces of domain,						dynamic array
	int	   *BdyFaceGIDs;			// GLOBAL IDs of a boundary Face for one COMM neighbor,	dynamic array
	int		BdyFaceGNIDs[4];		// to hold the four GLOBAL IDs of a boundary Face
	int		iD, ixF;				// to loop over the domains, Faces
	
	int		ncomm = C2K_ncomm ( );	// get the # of COMM neighbor domains
	
	DomainIDs           = malloc (ncomm*sizeof(int));
	NumBdyFacesOfDomain = malloc (ncomm*sizeof(int));
	
	crecfar ( DomainIDs, NumBdyFacesOfDomain );	// get from CMG an array of domain IDs for COMM neighbors
												// & an array of the # of shared Faces with each domain
	
//---------- Assure domain ID array is in ascending order
	OrderTwoArraysByFirstArrayContent (DomainIDs, NumBdyFacesOfDomain, ncomm);
	
	
//---------- Find the maximum number of boundary Faces to process for a neighbor domain, and bdy elements
	int maxDomainBdyFaces = -1;			// to store max # of boundary Faces
	
	for ( iD = 0; iD < ncomm; iD++)							// loop over neighbor COMM domains ...
	{
		if ( NumBdyFacesOfDomain[iD] > maxDomainBdyFaces)	// ... looking for the maximum
			maxDomainBdyFaces = NumBdyFacesOfDomain[iD];
	}
	
//---------- start to build cbdycomm, one ORDERED domain at a time,			(ordered on MPI process ID)
//									  one ORDERED boundary Face at a time,	(ordered on GLOBAL Face ID)
//									  one ORDERED Face Node at a time.		(ordered on GLOBAL Node ID)

	int		ixShBdyElement;		// to track the location in cbdycomm of the next shared boundary element

	BdyFaceGIDs = malloc (maxDomainBdyFaces*sizeof(int));	// to store GLOBAL IDs for boundary Faces

	ixShBdyElement = 0;
	for ( iD = 0; iD < ncomm; iD++)				// loop over neighbor COMM domains ...
	{
		crecf ( DomainIDs[iD], BdyFaceGIDs);	// get from CMG the GLOBAL Face IDs for this COMM domain
		
		Sort_Ints ( BdyFaceGIDs, NumBdyFacesOfDomain[iD]);	// to synch COMM Face order with neighboring domain
		
		for (ixF = 0; ixF < NumBdyFacesOfDomain[iD]; ixF++)	// loop over the ordered boundary Faces
		{
			cfcnda ( BdyFaceGIDs[ixF], &BdyFaceGNIDs[0]);		// from CMG get the four global Node IDs of the Face
			
			Sort_Ints ( &BdyFaceGNIDs[0], 4);					// to synch COMM bdy element order with neighboring domain
						
			cbdycomm [ixShBdyElement+0] = BElemNumFromFaceAndNodeGIDs (BdyFaceGIDs[ixF], BdyFaceGNIDs[0]) + 1;
			cbdycomm [ixShBdyElement+1] = BElemNumFromFaceAndNodeGIDs (BdyFaceGIDs[ixF], BdyFaceGNIDs[1]) + 1;
			cbdycomm [ixShBdyElement+2] = BElemNumFromFaceAndNodeGIDs (BdyFaceGIDs[ixF], BdyFaceGNIDs[2]) + 1;
			cbdycomm [ixShBdyElement+3] = BElemNumFromFaceAndNodeGIDs (BdyFaceGIDs[ixF], BdyFaceGNIDs[3]) + 1;
			ixShBdyElement += 4;
		}
	}
	
	free (DomainIDs);
	free (NumBdyFacesOfDomain);
	free (BdyFaceGIDs);
}

//======================================================================
void C2K_bdytoc (int *bdytoc)	// bdy element to LOCAL Corner & CFace pair #
{
	int		NumBE, iBE, ix;		// # of boundary elements, loop index, storage pointer for the 2D array bdytoc
	
	NumBE = getNumBE( );
	
	ix = 0;
	for (iBE = 0; iBE < NumBE; iBE++)
	{
		bdytoc[ix    ] = pBElem(iBE)->pCorner->nCornerLID + 1;	// 1-origin local Corner ID for the boundary element
		bdytoc[ix + 1] = -1;									// CAN'T SET THIS UNTIL I LEARN THE CFACE PAIR RULE
	}
}



