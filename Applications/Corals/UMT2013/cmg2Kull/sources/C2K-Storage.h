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
#ifndef C2KSTORAGE
#define C2KSTORAGE

#define MAX_NUM_NEIGHBORS 6
#define MAX_NUM_BOUNDARY_FACE_TAGS 12

struct		   Location3D {double  x, y, z;};	// coordinates in 3D cartesian space
typedef struct Location3D	Loc3DTD;

typedef struct Zone			*ZonePtr;
typedef struct Face			*FacePtr;
typedef struct Side			*SidePtr;
typedef struct Edge			*EdgePtr;
typedef struct Node			*NodePtr;
typedef struct Corner		*CornerPtr;
typedef struct BElem		*BElemPtr;

typedef enum FB_States	//-------------------- Possible Face Boundary States (BOTH Zone & Corner Faces)
{ F_UNASSIGNED = 0, INTERIOR_FACE, SEND_FACE, RECEIVE_FACE, PROBLEM_BOUNDARY_FACE 
} FB_States;

typedef enum FS_States	//-------------------- Possible Face sorting "states"
{ S_UNASSIGNED = 0, S_LEFT, S_RIGHT
} FS_States;

typedef enum ZSD_States	//-------------------- Possible Zone sub-division "states"
{ DONT_SUBDIVIDE = 0, SUBDIVISION_OK
} ZSD_States;

//============================== GENERIC LIST TYPE DEFINITIONS
typedef enum umEntity
{ umZONE = 0, umFACE, umSIDE, umEDGE, umNODE, umCORNER, umUNDEFINED
} umEntity;

typedef struct  ListItem	*LItemPtr;	// Generic lists; to replace Node & Side lists
	
struct ListItem { // Used for generic lists; Edges first, eventually ... all
	umEntity	ListType;				// The type of entity in the list
	void		*pItem;					// Many list items may point to this same entity
	LItemPtr	pNextItem, pPriorItem;	// Links in the entity list
	LItemPtr	pNext    , pPrior    ;	// Links for the "real" list and "free" stack
	int			nLItemID;				// only used locally? not yet marked LID
	};

struct Zone { //============================== ZONE STRUCT AND LISTS
	FacePtr		p1stFace;	// first Face in the Zone's UNORDERED "Face list"
	CornerPtr	p1stCorner;	// first Corner in the Zone's UNORDERED "Corner list"
	SidePtr		p1stSide;	// first Side in the Zone's UNORDERED "Side list"
	ZSD_States	SubDivisionControl;
	Loc3DTD		Centroid;	// the Zone's physical centroid in 3D space
	//---------- ID & list pointers
	int			nZoneLID,	// Local ID; i.e., on a single domain
				nZoneGID;	// Global ID; unique over all parallel domains
	ZonePtr		pNextZone,	// next in the list of real or "free" Zones
				pPriorZone;	// used only in the doubly-linked real list
	};

struct Face { //============================== FACE STRUCT AND LISTS
	ZonePtr		pZone;		// the Zone that "owns" this Face
	SidePtr		p1stSide;	// should be ORDERED CCW on Face wrt Zone center
	LItemPtr	p1stNodeLI;	// ditto
	LItemPtr	p1stCornerLI;	// CCW list of Corners on the Face
	FacePtr		pOppositeFace;	// NULL if boundary, otherwise -> opposite Face
	FS_States	SortState;		// Marker for sorting Faces when dividing a Zone
	Loc3DTD		Centroid;		// the Face's physical centroid in 3D space
	FB_States	FType;			// the type of Face; interior, COMM, problem boundary
	//---------- ID & list pointers
	int			nFaceLID,	// Local ID; i.e., on a single domain
				nFaceGID;	// Global ID; unique over all parallel domains (ONLY GOOD FOR BOUNDARY FACES)
  int       FaceTagIndex; // index into DBF_Tags array defining what tag this boundary face has (0 if not tagged).
	FacePtr		pZsNextF,	// pointer to the next Face in a Zone's Face list
				pNextFace,	// next in the list of real or "free" Faces
				pPriorFace;	// used only in the doubly-linked real list
	};

struct Side { //============================== SIDE STRUCTS AND LISTS
	ZonePtr		pZone;			// the Zone that "owns" this Side
	FacePtr		pFace;			// the Face that this Side "sits" on
	EdgePtr		pEdge;			// the Edge that this Side is on
	NodePtr		pNode1,			// the right Node wrt feet at Face center, head at Zone center
				pNode2;			// the left Node wrt feet at Face center, head at Zone center
	CornerPtr	pCorner1,		// the Corner @ Node1 to which this Side contributes
				pCorner2;		// the Corner @ Node2 to which this Side contributes
	SidePtr		pTopSide,		// the Side "above" this one
				pOppositeSide,	// the Side on the other side of the Face that this side sits on
				pRightSide,		// the Side to right wrt feet at Face center, head at Zone center
				pLeftSide;		// the Side to left wrt feet at Face center, head at Zone center
	//---------- ID & list pointers
	int			nSideLID,	// Local ID; i.e., on a single domain
				nSideGID;	// Global ID; unique over all parallel domains
	SidePtr		pZsNextS,	// for a zone's side list
				pFsNextS,	// for a face's ORDERED side list
				pEsNextS,	// for an edge's side list
				pNextSide,	// next in the list of real or "free" Sides
				pPriorSide;	// used only in the doubly-linked real list
	};

struct Edge { //============================== EDGE STRUCT AND LISTS
	NodePtr		pNode1,		// Edge's start Node; different than Side's Node1
				pNode2;		// Edge's end Node; different than Side's Node2
	SidePtr		p1stSide;	// first Side of Edge's UNORDERED "Side list"
	CornerPtr	p1stCorner;	// first Corner of Edge's UNORDERED "Corner list"
	//---------- ID & list pointers
	int			nEdgeLID,	// Local ID; i.e., on a single domain
				nEdgeGID;	// Global ID; unique over all parallel domains
	EdgePtr		pNextEdge,	// next in the list of real or "free" Edges
				pPriorEdge;	// used only in the doubly-linked real list
	};

struct Node { //============================== NODE STRUCTS AND LISTS
	Loc3DTD		Loc3D;		// the Node's physical location in 3D space
	LItemPtr	p1stSideLI;	// first Side of Node's UNORDERED "Side list"
	LItemPtr	p1stEdgeLI;	// first Edge of Node's UNORDERED "Edge list"
	LItemPtr	p1stFaceLI;	// first Face of Node's UNORDERED "Face list"
	CornerPtr	p1stCorner;	// first Corner of the Node's UNORDERED "Corner list"
	//---------- ID & list pointers
	int			nNodeLID,	// Local ID; i.e., on a single domain
				nNodeGID;	// Global ID; unique over all parallel domains
	NodePtr		pNextNode,	// next in the list of real or "free" Nodes
				pPriorNode;	// used only in the doubly-linked real list
	};

struct Corner { //============================== CORNER STRUCT AND LISTS
	ZonePtr		pZone;			// Zone that "contains" this Corner
	NodePtr		pNode;			// Node that touches this Corner
	LItemPtr	p1stSideLI;		// first Side that touches this Corner
	LItemPtr	p1stFaceLI;		// first Face of Corner's UNORDERED "Face list"
	//---------- ID & list pointers
	int			nCornerLID,		// Local ID; i.e., on a single domain
				nCornerGID;		// Global ID; unique over all parallel domains
	CornerPtr	pZsNextC,		// pointer the next Corner of the Zone
				pNsNextC,		// pointer to the next Corner @ the same Node
				pNextCorner,	// next in the list of real or "free" Corners
				pPriorCorner;	// used only in the doubly-linked real list
	};

struct BElem { //============================== BOUNDARY ELEMENT STRUCT AND LISTS
	CornerPtr	pCorner;		// Corner that this boundary element connects to
	FacePtr		pFace;			// Face that this boundary element sits on
	//---------- ID & list pointers
	int			nBElemLID;		// Local ID for this boundary element
	BElemPtr	pNextBElem,		//
				pPriorBElem;	// 
	};

//======================================================================
// Define accessor functions for mesh storage.
//======================================================================
int	getNumZ ( );	ZonePtr	  pZone   ( int );
int	getNumE ( );	EdgePtr	  pEdge   ( int );
int	getNumN ( );	NodePtr	  pNode   ( int );
int	getNumF ( );	FacePtr	  pFace   ( int );
int	getNumS ( );	SidePtr	  pSide   ( int );
int	getNumC ( );	CornerPtr pCorner ( int );
int	getNumLI( );	LItemPtr  pListI  ( int );
int	getNumBE( );	BElemPtr  pBElem  ( int );		void setNumBE(int NumBE_Size );	// separate initialization

void setNumE( int newNumE );

int getMaxNumZ( );
int getMaxNumE( );
int getMaxNumN( );
int getMaxNumF( );
int getMaxNumS( );
int getMaxNumC( );

char**     getBoundaryFaceTags( );

//======================================================================
// Define accessor functions for communication data storage.
//======================================================================
int* getNumberOfSendNeighbors();
int* getSendNeighborIDs();
int* getNumSendFaces();
FacePtr* getSendFaces( int );

int* getNumberOfReceiveNeighbors();
int* getReceiveNeighborIDs();
int* getNumReceiveFaces();
FacePtr* getReceiveFaces( int );

//======================================================================
void	InitializeAllStorage ( );
void  FreeAllStorage( );

//======================================================================
ZonePtr		allocateZone      ( int i );
FacePtr		allocateFace      ( int i );
SidePtr		allocateSide      ( int i );
EdgePtr		allocateEdge      ( int i );
NodePtr		allocateNode      ( int i );
CornerPtr	allocateCorner    ( int i );
LItemPtr	allocateListItem  ( );	void freeListItem  ( LItemPtr pLI );
BElemPtr	allocateBElem     ( );

#endif
