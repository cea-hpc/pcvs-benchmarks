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
#include <math.h>
#include <string.h>

#include "C2K-CMG.h"
#include "C2K-Storage.h"
#include "C2K-KC_Create.h"

//======================================================================
// Define storage arrays.
//======================================================================

int			c2k_numZones;
struct Zone	*Zones;
ZonePtr		*pZones;

int			numEdges;
struct Edge	*Edges;
EdgePtr		*pEdges;

int			numNodes;
struct Node	*Nodes ;	// Could be sized and dynamically allocated ONCE
NodePtr		*pNodes;

int			numFaces;
struct Face	*Faces;
FacePtr		*pFaces;

int			numSides;
struct Side	*Sides;
SidePtr		*pSides;

int				numCorners;
struct Corner	*Corners;
CornerPtr		*pCorners;

//--------------------------------------------------------------------------------
char* DBF_Tags[MAX_NUM_BOUNDARY_FACE_TAGS+1]; // collection of all tag names defined on domain boundary faces

//======================================================================
//  Dynamically allocated list of structs; many additions, some deletions
//======================================================================

int				createdListItems = 0;	// total # created
int				numListItems     = 0;	// total "in use" at any given time
LItemPtr		pFirstListItem = NULL;	// pointer to list of "in use" LItems
LItemPtr		pFreeListItem  = NULL;	// pointer to top of stack of "free" LItems

//======================================================================
// Dynamically allocated array; sized ONCE
//======================================================================

int				c2k_numBE_size;	// Size of storage available to C2K
int				c2k_numBE;		// # BElem actually allocated to C2K
BElemPtr		pBElems;		// pointer to a dynamic array of boundary elements

int       numSendNeighbors;
int       *SendNeighborIDs;
int       *NumSendFaces;
FacePtr   **SendFaces;

int       numReceiveNeighbors;
int       *ReceiveNeighborIDs;
int       *NumReceiveFaces;
FacePtr   **ReceiveFaces;

int	getNumZ ( ) { return ( c2k_numZones );}
int	getNumE ( ) { return ( numEdges     );}
int	getNumN ( ) { return ( numNodes     );}
int	getNumF ( ) { return ( numFaces     );}
int	getNumS ( ) { return ( numSides     );}
int	getNumC ( ) { return ( numCorners   );}
int	getNumBE( ) { return ( c2k_numBE    );}

void setNumE( int newNumE ) { numEdges = newNumE; }

ZonePtr	  pZone   ( int i ) { return ( pZones    [i] );}
EdgePtr	  pEdge   ( int i ) { return ( pEdges    [i] );}
NodePtr	  pNode   ( int i ) { return ( pNodes    [i] );}
FacePtr	  pFace   ( int i ) { return ( pFaces    [i] );}
SidePtr	  pSide   ( int i ) { return ( pSides    [i] );}
CornerPtr pCorner ( int i ) { return ( pCorners  [i] );}
BElemPtr  pBElem  ( int i ) { return ( pBElems  + i );}

char**     getBoundaryFaceTags( ) { return &DBF_Tags[0]; }

void setNumBE (int NumBE_Size )
{
	int		iBE;	// to loop over boundary element storage during initialization
	
	c2k_numBE_size = NumBE_Size;	// remember max size to protect agains overflow
	c2k_numBE = 0;					// initialize allocated boundary element # to zero
	
	pBElems = malloc ( NumBE_Size * sizeof (struct BElem) );	// dynamic memory allocation
	
	for (iBE = 0; iBE < NumBE_Size; iBE++)		// loop over all boundary element struct storage ...
	{
		(pBElems + iBE)->pCorner     = NULL;		// ..., and initialize all struct fields
		(pBElems + iBE)->pFace       = NULL;
		(pBElems + iBE)->nBElemLID   = iBE;
		(pBElems + iBE)->pNextBElem  = NULL;
		(pBElems + iBE)->pPriorBElem = NULL;
	}
	
}

int* getNumberOfSendNeighbors() { return &numSendNeighbors; }
int* getSendNeighborIDs(){ return &SendNeighborIDs[0]; }
int* getNumSendFaces(){ return &NumSendFaces[0]; }
FacePtr* getSendFaces( int domID )
{
    SendNeighborIDs = getSendNeighborIDs();
    
    int i;
    /* find index of SendNeighborIDs which is = domID */
    for(i=0; i< MAX_NUM_NEIGHBORS ; i++)
    {
        assert(SendNeighborIDs[i] != -1 );
        
        if( SendNeighborIDs[i] == domID )
            break;
    }
    if( i ==MAX_NUM_NEIGHBORS )
        printf("  ERROR in getSendFaces, domID=%d not found in SendNeighborIDs.\n",domID);
    
    return SendFaces[i];
}

int* getNumberOfReceiveNeighbors() { return &numReceiveNeighbors; }
int* getReceiveNeighborIDs(){ return &ReceiveNeighborIDs[0]; }
int* getNumReceiveFaces(){ return &NumReceiveFaces[0]; }
FacePtr* getReceiveFaces( int domID )
{
  int i;
  /* find index of ReceiveNeighborIDs which is = domID */
  for(i=0; i< MAX_NUM_NEIGHBORS ; i++)
  {
    if( ReceiveNeighborIDs[i] == domID )
      break;
  }
  if( i ==MAX_NUM_NEIGHBORS )
    printf("  ERROR in getReceiveFaces, domID=%d not found in ReceiveNeighborIDs.\n",domID);
  
  return ReceiveFaces[i];
}

void	InitializeAllStorage ( )
//======================================================================
// Set all pointer arrays to point to corresponding array elements.
// Set unique IDs in ascending order.
//======================================================================
{	int			ix,nbr;
	LItemPtr	pLI, pLI2free;	// to follow pointers of List Items to free
  
  //
  // access cmg to determine how many actual initial nodes and zones we have in this mesh
  cnmnoda(&numNodes);
  cnumzns(&c2k_numZones);
  
  Zones = (struct Zone*)malloc(c2k_numZones*sizeof(struct Zone) );
  assert( Zones != NULL);
  pZones = (ZonePtr*)malloc(c2k_numZones*sizeof(ZonePtr) );
  assert( pZones != NULL);
  
	for (ix = 0; ix < c2k_numZones; ix++)
	{	pZones[ix] = &Zones[ix];
		Zones[ix].nZoneLID    = ix;
		Zones[ix].nZoneGID    = -1;
		Zones[ix].p1stFace   = NULL;
		Zones[ix].p1stCorner = NULL;
		Zones[ix].p1stSide   = NULL;
		Zones[ix].pNextZone  = NULL;
		Zones[ix].pPriorZone = NULL;
		Zones[ix].SubDivisionControl = DONT_SUBDIVIDE;
		Zones[ix].Centroid.x = -1.0;
		Zones[ix].Centroid.y = -1.0;
		Zones[ix].Centroid.z = -1.0;
	}

  /* numEdges = 3*numNodes is an over estimate. */
	numEdges = 3*numNodes;
  
  Edges = (struct Edge*)malloc(numEdges*sizeof(struct Edge) );
  assert( Edges != NULL );
  pEdges = (EdgePtr*)malloc(numEdges*sizeof(EdgePtr) );
  assert( pEdges != NULL );
  
	for (ix = 0; ix < numEdges; ix++)
	{	pEdges[ix] = &Edges[ix];
		Edges[ix].nEdgeLID    = ix;
		Edges[ix].nEdgeGID    = -1;
		Edges[ix].pNode1     = NULL;
		Edges[ix].pNode2     = NULL;
		Edges[ix].p1stSide   = NULL;
		Edges[ix].p1stCorner = NULL;
		Edges[ix].pNextEdge  = NULL;
		Edges[ix].pPriorEdge = NULL;
	}

  Nodes = (struct Node*)malloc(numNodes*sizeof(struct Node) );
  assert( Nodes != NULL );
  pNodes = (NodePtr*)malloc(numNodes*sizeof(NodePtr) );
  assert( pNodes != NULL );
  
	for (ix = 0; ix < numNodes; ix++)
	{	pNodes[ix] = &Nodes[ix];
		Nodes[ix].nNodeLID    = ix;
		Nodes[ix].nNodeGID    = -1;
		Nodes[ix].p1stSideLI = NULL;
		Nodes[ix].p1stEdgeLI = NULL;
		Nodes[ix].p1stFaceLI = NULL;
		Nodes[ix].p1stCorner = NULL;
		Nodes[ix].pNextNode  = NULL;
		Nodes[ix].pPriorNode = NULL;
		Nodes[ix].Loc3D.x    = -1.0;
		Nodes[ix].Loc3D.y    = -1.0;
		Nodes[ix].Loc3D.z    = -1.0;
	}

  numFaces = 6*c2k_numZones;
  Faces = (struct Face*)malloc(numFaces * sizeof(struct Face) );
  assert( Faces != NULL );
  pFaces = (FacePtr*)malloc(numFaces * sizeof(FacePtr) );
  assert( pFaces != NULL);
  
	for (ix = 0; ix < numFaces; ix++)
	{	pFaces[ix] = &Faces[ix];
		Faces[ix].nFaceLID       = ix;
		Faces[ix].nFaceGID       = -1;
		Faces[ix].FaceTagIndex   = 0;
		Faces[ix].pZone         = NULL;
		Faces[ix].p1stSide      = NULL;
		Faces[ix].p1stNodeLI    = NULL;
		Faces[ix].p1stCornerLI  = NULL;
		Faces[ix].pOppositeFace = NULL;
		Faces[ix].pZsNextF      = NULL;
		Faces[ix].pNextFace     = NULL;
		Faces[ix].pPriorFace    = NULL;
		Faces[ix].Centroid.x = -1.0;
		Faces[ix].Centroid.y = -1.0;
		Faces[ix].Centroid.z = -1.0;
		Faces[ix].FType      = F_UNASSIGNED;
	}

  numSides = 24*c2k_numZones;
  Sides = (struct Side*)malloc(numSides*sizeof(struct Side));
  assert( Sides != NULL);
  pSides = (SidePtr*)malloc(numSides*sizeof(SidePtr));
  assert( pSides != NULL);
  
	for (ix = 0; ix < numSides; ix++)
	{	pSides[ix] = &Sides[ix];
		Sides[ix].nSideLID       = ix;
		Sides[ix].nSideGID       = -1;
		Sides[ix].pZone         = NULL;
		Sides[ix].pFace         = NULL;
		Sides[ix].pEdge         = NULL;
		Sides[ix].pNode1        = NULL;
		Sides[ix].pNode2        = NULL;
		Sides[ix].pCorner1      = NULL;
		Sides[ix].pCorner2      = NULL;
		Sides[ix].pTopSide      = NULL;
		Sides[ix].pOppositeSide = NULL;
		Sides[ix].pRightSide    = NULL;
		Sides[ix].pLeftSide     = NULL;
		Sides[ix].pZsNextS      = NULL;
		Sides[ix].pFsNextS      = NULL;
		Sides[ix].pEsNextS      = NULL;
		Sides[ix].pNextSide     = NULL;
		Sides[ix].pPriorSide    = NULL;
	}

  numCorners = 8*c2k_numZones;
  Corners = (struct Corner*)malloc(numCorners*sizeof(struct Corner));
  assert( Corners != NULL );
  pCorners = (CornerPtr*)malloc(numCorners*sizeof(CornerPtr));
  assert( pCorners != NULL );
  
	for (ix = 0; ix < numCorners; ix++)
	{	pCorners[ix] = &Corners[ix];
		Corners[ix].nCornerLID    = ix;
		Corners[ix].nCornerGID    = -1;
		Corners[ix].pZone        = NULL;
		Corners[ix].pNode        = NULL;
		Corners[ix].p1stSideLI   = NULL;
		Corners[ix].p1stFaceLI   = NULL;
		Corners[ix].pZsNextC     = NULL;
		Corners[ix].pNsNextC     = NULL;
		Corners[ix].pNextCorner  = NULL;
		Corners[ix].pPriorCorner = NULL;
	}

  //---------- Allocate comm data

  SendNeighborIDs = (int*)malloc(MAX_NUM_NEIGHBORS * sizeof(int) );
  assert( SendNeighborIDs != NULL );
  for(ix=0;ix<MAX_NUM_NEIGHBORS ; ix++)
  {
      SendNeighborIDs[ix]=-1;
  }
  
  NumSendFaces= (int*)malloc(MAX_NUM_NEIGHBORS * sizeof(int) );
  assert( NumSendFaces != NULL );
  for(ix=0;ix<MAX_NUM_NEIGHBORS ; ix++)
  {
      NumSendFaces[ix]=-1;
  }
  
  ReceiveNeighborIDs = (int*)malloc(MAX_NUM_NEIGHBORS * sizeof(int) );
  assert( ReceiveNeighborIDs != NULL );
  for(ix=0;ix<MAX_NUM_NEIGHBORS ; ix++)
  {
      ReceiveNeighborIDs[ix]=-1;
  }
  
  NumReceiveFaces= (int*)malloc(MAX_NUM_NEIGHBORS * sizeof(int) );
  assert( NumReceiveFaces != NULL );
  for(ix=0;ix<MAX_NUM_NEIGHBORS ; ix++)
  {
      NumReceiveFaces[ix]=-1;
  }

  SendFaces = (FacePtr**)malloc(MAX_NUM_NEIGHBORS*sizeof(FacePtr*));
  assert( SendFaces != NULL );
  ReceiveFaces = (FacePtr**)malloc(MAX_NUM_NEIGHBORS*sizeof(FacePtr*));
  assert( ReceiveFaces != NULL );
  
  for(nbr=0;nbr<MAX_NUM_NEIGHBORS;nbr++)
  {
    SendFaces[nbr] = (FacePtr*)malloc(numNodes*sizeof(FacePtr));
    assert( SendFaces[nbr] != NULL );
    ReceiveFaces[nbr] = (FacePtr*)malloc(numNodes*sizeof(FacePtr));
    assert( ReceiveFaces[nbr] != NULL );
    for(ix=0;ix<numNodes;ix++)
    {
      SendFaces[nbr][ix] = NULL;
      ReceiveFaces[nbr][ix] = NULL;
    }
  }
  
	//---------- NEED TO FREE ALL LIST LITEMS THAT WERE ALLOCATED, IF ANY
	pLI = pFreeListItem;
	while (pLI != NULL)
	{	pLI2free = pLI;
		pLI = pLI->pNextItem;
		free(pLI2free);
	}
		
	pLI = pFirstListItem;
	while (pLI != NULL)
	{	pLI2free = pLI;
		pLI = pLI->pNextItem;
		free(pLI2free);
	}
		
	createdListItems = 0;	// total list items created
	numListItems     = 0;	// # of list items in use at any given time
	pFirstListItem = NULL;	// pointer to list of "in use" LItems
	pFreeListItem  = NULL;	// pointer to top of stack of "free" LItems

  //------------- Build formerly static C2K-CK_Create.c static data dynamically
  int **pGNID = getGNID();
  *pGNID = (int*)malloc(numNodes*sizeof(int) );
  assert( *pGNID != NULL );
  int *GNID;
  GNID = *pGNID;
  
  int **pGZID = getGZID();
  *pGZID = (int*)malloc(c2k_numZones * sizeof(int) );
  assert( *pGZID != NULL );
  int *GZID;
  GZID = *pGZID;
  
  int ***pLZsLNs = getLZsLNs();
  *pLZsLNs = (int**)malloc(c2k_numZones * sizeof(int*) );
  assert( *pLZsLNs != NULL );
  int **LZsLNs;
  LZsLNs = *pLZsLNs;
  for(ix=0;ix<c2k_numZones;ix++)
  {
    LZsLNs[ix] = (int*)malloc(8*sizeof(int));
    assert( LZsLNs[ix] != NULL );
  }
  
  int **pLZsKC_Type = getLZsKC_Type();
  *pLZsKC_Type = (int*)malloc(c2k_numZones * sizeof(int) );
  assert( *pLZsKC_Type != NULL );
  int *LZsKC_Type;
  LZsKC_Type = *pLZsKC_Type;
  
  int **pCN_DomainID = getCN_DomainID();
  *pCN_DomainID = (int*)malloc(MAX_NUM_NEIGHBORS * sizeof(int) );
  assert( *pCN_DomainID != NULL );
  int *CN_DomainID;
  CN_DomainID = *pCN_DomainID;
  
  int **pCN_NumFaces = getCN_NumFaces();
  *pCN_NumFaces = (int*)malloc(MAX_NUM_NEIGHBORS * sizeof(int) );
  assert( *pCN_NumFaces != NULL );
  int *CN_NumFaces;
  CN_NumFaces = *pCN_NumFaces;
  
  int **pCBF_GID = getCBF_GID();
  *pCBF_GID = (int*)malloc(numFaces * sizeof(int) );
  assert( *pCBF_GID != NULL );
  int *CBF_GID;
  CBF_GID = *pCBF_GID;
  
  int **pCBF_LID = getCBF_LID();
  *pCBF_LID = (int*)malloc(numFaces * sizeof(int) );
  assert( *pCBF_LID != NULL );
  int *CBF_LID;
  CBF_LID = *pCBF_LID;
  
  int **pDBF_GID = getDBF_GID();
  *pDBF_GID = (int*)malloc(numFaces * sizeof(int) );
  assert( *pDBF_GID != NULL );
  int *DBF_GID;
  DBF_GID = *pDBF_GID;
  
  int **pDBF_LID = getDBF_LID();
  *pDBF_LID = (int*)malloc(numFaces * sizeof(int) );
  assert( *pDBF_LID != NULL );
  int *DBF_LID;
  DBF_LID = *pDBF_LID;
  
/* 	printf("\n\n***** KC storage initialization completed *****\n"); */
}

void	FreeAllStorage ( )
{
  int ix,nbr;
  int **pGNID, **pGZID, **pLZsKC_Type, **pCN_DomainID;
  int **pCN_NumFaces, **pCBF_GID, **pCBF_LID, **pDBF_GID, **pDBF_LID;
  int ***pLZsLNs;
  int **LZsLNs;
  int numMeshTags=-1,tagIndex=0;
  int tagType;
  
  cmtgs(&numMeshTags);  // get the total number of mesh tags for this domain
  free(DBF_Tags[tagIndex++]);
  
  for(ix=0;ix<numMeshTags;ix++)
  {
    cmtgtp(ix,&tagType);  // get integer tag type from CMG (0-node, 1-edge, 2-face, 3-zone, 4-material)

    if( tagType==2 ) // only handle face tags
      free(DBF_Tags[tagIndex++]);
  }

  free(pBElems);
  
 // Deallocate all the list data
  pGNID = getGNID();
  pGZID = getGZID();
  pLZsKC_Type = getLZsKC_Type();
  pCN_DomainID = getCN_DomainID();
  pCN_NumFaces = getCN_NumFaces();
  pCBF_GID = getCBF_GID();
  pCBF_LID = getCBF_LID();
  pDBF_GID = getDBF_GID();
  pDBF_LID = getDBF_LID();
  pLZsLNs = getLZsLNs();
  LZsLNs = *pLZsLNs;
  
  free(*pDBF_LID);
  free(*pDBF_GID);
  free(*pCBF_LID);
  free(*pCBF_GID);
  free(*pCN_NumFaces);
  free(*pCN_DomainID);
  free(*pLZsKC_Type);
  for(ix=0;ix<getNumZ();ix++)
  {
    free(LZsLNs[ix]);
  }
  free(*pLZsLNs);
  free(*pGZID);
  free(*pGNID);
  for(nbr=0;nbr<MAX_NUM_NEIGHBORS;nbr++)
  {
    free( SendFaces[nbr] );
    free( ReceiveFaces[nbr]);
  }
  free(SendFaces);
  free(ReceiveFaces);
  free(NumReceiveFaces);
  free(ReceiveNeighborIDs);
  free(NumSendFaces);
  free(SendNeighborIDs);
  free(pCorners);
  free(Corners);
  free(pSides);
  free(Sides);
  free(pFaces);
  free(Faces);
  free(pNodes);
  free(Nodes);
  free(pEdges);
  free(Edges);
  free(pZones);
  free(Zones);

  c2k_numZones=0;
  numNodes=0;
  numEdges=0;
  numFaces=0;
  numSides=0;
  numCorners=0;
}

void initializeLItemStruct (LItemPtr pLI)
{
	createdListItems++;		// this variable is only incremented
	
	pLI->nLItemID   = createdListItems;
	pLI->ListType   = umUNDEFINED;
	pLI->pItem      = NULL;
	pLI->pNextItem  = NULL;
	pLI->pPriorItem = NULL;
	pLI->pNext      = NULL;
	pLI->pPrior     = NULL;
}

int	newIndex;

ZonePtr		allocateZone   (int i )
{
	if (i >= getNumZ() )
	{	printf("ERROR Out of Zone storage.\n");
		exit(1);
	}

	if (i > 0)
	{	Zones[i  ].pPriorZone = pZones[i-1];
		Zones[i-1].pNextZone  = pZones[i  ];
	}

	return (pZones[i]);
}

FacePtr		allocateFace   ( int i )
{
	if (i >= getNumF() )
	{	printf("ERROR Out of Face storage.\n");
		exit(1);
	}
	if (i > 0)
	{	Faces[i  ].pPriorFace = pFaces[i-1];
		Faces[i-1].pNextFace  = pFaces[i  ];
	}

	return (pFaces[i]);
}

SidePtr		allocateSide   ( int i )
{
	if (i >= getNumS() )
	{	printf("ERROR Out of Side storage, requesting %d out of a max of %d.\n",i,getNumS());
		exit(1);
	}
	if (i > 0)
	{	Sides[i  ].pPriorSide = pSides[i-1];
		Sides[i-1].pNextSide  = pSides[i  ];
	}
  
	return (pSides[i]);
}

EdgePtr		allocateEdge   ( int i )
{
	if (i >= getNumE() )
	{	printf("ERROR Out of Edge storage.\n");
		exit(1);
	}

	if (i > 0)
	{	Edges[i  ].pPriorEdge = pEdges[i-1];
		Edges[i-1].pNextEdge  = pEdges[i  ];
	}

	return (pEdges[i]);
}

NodePtr		allocateNode   ( int i )
{
	if (i >= getNumN() )
	{	printf("ERROR Out of Node storage: need %d nodes, only have %d.\n",i,numNodes);
		exit(1);
	}
  
	if (i > 0)
	{	Nodes[i  ].pPriorNode = pNodes[i-1];
		Nodes[i-1].pNextNode  = pNodes[i  ];
	}
	return (pNodes[i]);
}

CornerPtr	allocateCorner ( int i )
{
	if (i >= getNumC() )
	{	printf("ERROR Out of Corner storage.\n");
		exit(1);
	}
	if (i > 0)
	{	Corners[i  ].pPriorCorner = pCorners[i-1];
		Corners[i-1].pNextCorner  = pCorners[i  ];
	}
  
	return (pCorners[i]);
}

/*
LItemPtr	allocateListItem  ( )
{
	if (numListItems >= MAX_LIST_ITEMS)
	{	printf("ERROR Out of ListItem storage.\n");
		exit(1);
	}
	if (numListItems > 0)
	{	ListItems[numListItems  ].pPrior = pListItems[numListItems-1];
		ListItems[numListItems-1].pNext  = pListItems[numListItems  ];
	}
	newIndex = numListItems; 
	numListItems++;
	return (pListItems[newIndex]);
}
*/

LItemPtr	allocateListItem  ( )
{
	LItemPtr pLI;
	
	pLI = malloc (sizeof(struct ListItem));	// struct
	
	if (pLI == NULL)
	{	printf("ERROR List Item struct allocation failed.\n");
		exit(1);
	}
	initializeLItemStruct (pLI);	numListItems++;	// to track active #
	
	pLI->pNext = pFirstListItem;	// push onto a stack of actively used LItems
	pFirstListItem = pLI;
	
	return (pLI);
}

void freeListItem  ( LItemPtr pLI )
{
	LItemPtr	pPrior;	// to loop over the list of "in use" List Items
	
	//----- remove LItem from the "in use" list and place on "free" stack
	if ( pFirstListItem == NULL)
	{
		printf ("ERROR - trying to delete from an empty list.\n");
		exit (1);
	}
	
	if ( pLI == pFirstListItem )
	{
		pFirstListItem = pLI->pNextItem;	// remove from "in use" list
		pLI->pNextItem = pFreeListItem;		// push onto "free" stack
		pFreeListItem  = pLI;
	}
	else
	{	pPrior = pFirstListItem;
		while ( pPrior->pNextItem != NULL )
		{
			if (pPrior->pNextItem == pLI)
			{
				pPrior->pNextItem = pLI->pNextItem;	// remove from "in use" list
				pLI->pNextItem = pFreeListItem;		// push onto "free" stack
				pFreeListItem  = pLI;
				break;
			}
			pPrior = pPrior->pNextItem;
		}
	}
	
	numListItems--;	// to track active #
}

BElemPtr allocateBElem ( )
{
	if (c2k_numBE >= c2k_numBE_size)
	{	printf("ERROR Out of boundary element storage.\n");
		exit(1);
	}
	if (c2k_numBE > 0)
	{	(pBElems + c2k_numBE  )->pPriorBElem = pBElems + c2k_numBE - 1;
		(pBElems + c2k_numBE-1)->pNextBElem  = pBElems + c2k_numBE    ;
	}
	newIndex = c2k_numBE;
	c2k_numBE++;
	return (pBElems+newIndex);
}

