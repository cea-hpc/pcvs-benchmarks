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
#include "C2K-Lists.h"
#include "C2K-KC_Create.h"
#include "C2K-KC_API.h"

//============================================================ NODE related functions
int Number_Of_Owned_Nodes ( )
{
	return ( getNumN() );
}

int	Local_NodeID_From_Ptr (NodePtr pN)
{
	return (pN->nNodeLID);
}

int	Global_NodeID_From_Ptr (NodePtr pN)
{
	return (pN->nNodeGID);
}

//------------------------------------------------------------ Looping over OWNED NODES
NodePtr	Owned_Node_Last_Accessed;	// global storage for sequential access

NodePtr	Owned_Node_Begin ( )
{
	Owned_Node_Last_Accessed = pNode(0);
	return (Owned_Node_Last_Accessed);
}

NodePtr	Owned_Node_End ( )
{
	return (NULL);
}

NodePtr	Next_Owned_Node ( )
{
	Owned_Node_Last_Accessed = Owned_Node_Last_Accessed->pNextNode;
	return (Owned_Node_Last_Accessed);
}

//============================================================ EDGE related functions
int Number_Of_Owned_Edges ( )
{
	return ( getNumE() );
}

int	Local_EdgeID_From_Ptr (EdgePtr pN)
{
	return (pN->nEdgeLID);
}

//------------------------------------------------------------ Looping over OWNED EDGES
EdgePtr	Owned_Edge_Last_Accessed;	// global storage for sequential access

EdgePtr	Owned_Edge_Begin ( )
{
	Owned_Edge_Last_Accessed = pEdge(0);
	return (Owned_Edge_Last_Accessed);
}

EdgePtr	Owned_Edge_End ( )
{
	return (NULL);
}

EdgePtr	Next_Owned_Edge ( )
{
	Owned_Edge_Last_Accessed = Owned_Edge_Last_Accessed->pNextEdge;
	return (Owned_Edge_Last_Accessed);
}

//============================================================ ZONE related functions
int Number_Of_Owned_Zones ( )
{
	return ( getNumZ() );
}

int Local_ZoneID_From_Ptr (ZonePtr pZ)
{
	return (pZ->nZoneLID);
}

//------------------------------------------------------------ Looping over OWNED ZONES
ZonePtr	Owned_Zone_Last_Accessed;	// global storage for sequential access

ZonePtr	Owned_Zone_Begin ( )
{
	Owned_Zone_Last_Accessed = pZone(0);
	return (Owned_Zone_Last_Accessed);
}

ZonePtr	Owned_Zone_End ( )
{
	return (NULL);
}

ZonePtr	Next_Owned_Zone ( )
{
	Owned_Zone_Last_Accessed = Owned_Zone_Last_Accessed->pNextZone;
	return (Owned_Zone_Last_Accessed);
}

//------------------------------------------------------------ Looping over a ZONE'S CORNERS
CornerPtr Zones_Corner_Last_Accessed;	// global storage for sequential access

CornerPtr Zones_Corner_Begin (ZonePtr pZ)
{
	Zones_Corner_Last_Accessed = pZ->p1stCorner;
	return (Zones_Corner_Last_Accessed);
}

CornerPtr	Zones_Corner_End (ZonePtr pZ )
{
	return (NULL);
}

CornerPtr	Zones_Next_Corner ( )
{
	Zones_Corner_Last_Accessed = Zones_Corner_Last_Accessed->pZsNextC;
	return (Zones_Corner_Last_Accessed);
}

//------------------------------------------------------------ Looping over a ZONE'S SIDES
SidePtr	Zones_Side_Last_Accessed;	// global storage for sequential access

SidePtr	Zones_Side_Begin (ZonePtr pZ)
{
	Zones_Side_Last_Accessed = pZ->p1stSide;
	return (Zones_Side_Last_Accessed);
}

SidePtr	Zones_Side_End (ZonePtr pZ )
{
	return (NULL);
}

SidePtr	Zones_Next_Side ( )
{
	Zones_Side_Last_Accessed = Zones_Side_Last_Accessed->pZsNextS;
	return (Zones_Side_Last_Accessed);
}

//------------------------------------------------------------ Looping over a ZONE'S FACES
FacePtr	Zones_Face_Last_Accessed;	// global storage for sequential access

FacePtr	Zones_Face_Begin (ZonePtr pZ)
{
	Zones_Face_Last_Accessed = pZ->p1stFace;
	return (Zones_Face_Last_Accessed);
}

FacePtr	Zones_Face_End (ZonePtr pZ )
{
	return (NULL);
}

FacePtr	Zones_Next_Face ( )
{
	Zones_Face_Last_Accessed = Zones_Face_Last_Accessed->pZsNextF;
	return (Zones_Face_Last_Accessed);
}

//============================================================ FACE related functions
int Number_Of_Owned_Faces ( )
{
	return ( getNumF() );
}

int Local_FaceID_From_Ptr (FacePtr pF)
{
	return (pF->nFaceLID);
}

int Global_FaceID_From_Ptr (FacePtr pF)
{
	return (pF->nFaceGID);
}

//------------------------------------------------------------ Looping over OWNED FACES
FacePtr	Owned_Face_Last_Accessed;	// global storage for sequential access

FacePtr	Owned_Face_Begin ( )
{
	Owned_Face_Last_Accessed = pFace(0);
	return (Owned_Face_Last_Accessed);
}

FacePtr	Owned_Face_End ( )
{
	return (NULL);
}

FacePtr	Next_Owned_Face ( )
{
	Owned_Face_Last_Accessed = Owned_Face_Last_Accessed->pNextFace;
	return (Owned_Face_Last_Accessed);
}

//------------------------------------------------------------ Looping over a FACE'S CORNERS
LItemPtr	FCL_Last_Accessed;		// global storage for sequential access
int			FCL_Index_Last_Accessed;// ditto

CornerPtr	Faces_Corner_Begin (FacePtr pF)
{
	FCL_Last_Accessed       = pF->p1stCornerLI;
	FCL_Index_Last_Accessed = 0;
	return (RetrieveListItem(FCL_Last_Accessed, umCORNER, FCL_Index_Last_Accessed) );
}

CornerPtr	Faces_Corner_End (FacePtr pF)
{
	return (NULL);
}

CornerPtr	Faces_Next_Corner ( )
{
	FCL_Index_Last_Accessed++;
	return (RetrieveListItem(FCL_Last_Accessed, umCORNER, FCL_Index_Last_Accessed) );
}

//------------------------------------------------------------ Looping over a FACE'S Sides
SidePtr Faces_Side_Last_Accessed;		// global storage for sequential access

SidePtr	Faces_Side_Begin (FacePtr pF)
{
	Faces_Side_Last_Accessed        = pF->p1stSide;
	return (Faces_Side_Last_Accessed );
}

SidePtr	Faces_Side_End (FacePtr pF)
{
	return (NULL);
}

SidePtr	Faces_Next_Side ( )
{
	Faces_Side_Last_Accessed = Faces_Side_Last_Accessed->pFsNextS;
	return (Faces_Side_Last_Accessed );
}

//------------------------------------------------------------ get the opposite Face
FacePtr	Faces_Opposite_Face (FacePtr pF)
{
	return (pF->pOppositeFace);
}

//------------------------------------------------------------ get the Boundary Face's Tag
char*     Get_Boundary_Face_Tag (FacePtr pF)
{
  char** tags = getBoundaryFaceTags();
  
	return ( tags[pF->FaceTagIndex] );
}

//============================================================ SIDE related functions
int Number_Of_Owned_Sides ( )
{
	return ( getNumS() );
}

int Local_SideID_From_Ptr (SidePtr pS)
{
	return (pS->nSideLID);
}

//------------------------------------------------------------ Looping over OWNED SIDES
SidePtr	Owned_Side_Last_Accessed;	// global storage for sequential access

SidePtr Owned_Side_Begin ( )
{
	Owned_Side_Last_Accessed = pSide(0);
	return (Owned_Side_Last_Accessed);
}

SidePtr Owned_Side_End ( )
{
	return (NULL);
}

SidePtr Next_Owned_Side ( )
{
	Owned_Side_Last_Accessed = Owned_Side_Last_Accessed->pNextSide;
	return (Owned_Side_Last_Accessed);
}

//------------------------------------------------------------ Return various pointers from a Side
ZonePtr Sides_Zone (SidePtr pS)
{
	return (pS->pZone);
}

FacePtr Sides_Face (SidePtr pS)
{
	return (pS->pFace);
}

CornerPtr Sides_Right_Corner (SidePtr pS)
{
	return (pS->pCorner1);
}

CornerPtr Sides_Left_Corner (SidePtr pS)
{
	return (pS->pCorner2);
}

SidePtr Sides_Opposite_Side (SidePtr pS)
{
	return (pS->pOppositeSide);
}

//============================================================ CORNER related functions
int Number_Of_Owned_Corners ( )
{
	return ( getNumC () );
}

int Local_CornerID_From_Ptr (CornerPtr pC)
{
	return (pC->nCornerLID);
}

//------------------------------------------------------------ Looping over OWNED CORNERS
CornerPtr	Owned_Corner_Last_Accessed;	// global storage for sequential access

CornerPtr Owned_Corner_Begin ( )
{
	Owned_Corner_Last_Accessed = pCorner(0);
	return (Owned_Corner_Last_Accessed);
}

CornerPtr Owned_Corner_End ( )
{
	return (NULL);
}

CornerPtr Next_Owned_Corner ( )
{
	Owned_Corner_Last_Accessed = Owned_Corner_Last_Accessed->pNextCorner;
	return (Owned_Corner_Last_Accessed);
}

//------------------------------------------------------------ Return various pointers from a Corner
NodePtr Corners_Node (CornerPtr pC)
{
	return (pC->pNode);
}

ZonePtr Corners_Zone (CornerPtr pC)
{
	return (pC->pZone);
}

//------------------------------------------------------------ Looping over a CORNER'S FACES
LItemPtr	CFL_Last_Accessed;		// global storage for sequential access
int			CFL_Index_Last_Accessed;// ditto

FacePtr Corners_Face_Begin (CornerPtr pC)
{
	CFL_Last_Accessed       = pC->p1stFaceLI;
	CFL_Index_Last_Accessed = 0;
	return (RetrieveListItem(CFL_Last_Accessed, umFACE, CFL_Index_Last_Accessed) );
}

FacePtr Corners_Face_End (CornerPtr pC)
{
	return (NULL);
}

FacePtr Corners_Next_Face ( )
{
	CFL_Index_Last_Accessed++;
	return (RetrieveListItem(CFL_Last_Accessed, umFACE, CFL_Index_Last_Accessed) );
}


//======================================== FACE communication functions


//------------------------------------------------------------- number of neighbors I send faces to
int Number_Of_Send_Neighbors( )
{
  return *(getNumberOfSendNeighbors());
}

//------------------------------------------------------------- number of neighbors I receive faces from
int     Number_Of_Receive_Neighbors( )
{
  return *(getNumberOfReceiveNeighbors());
}

//------------------------------------------------------------- arrays of IDs of neighbors I send/receive from

int* Send_Neighbor_IDs( )
{
  return getSendNeighborIDs();
}

int* Receive_Neighbor_IDs( )
{
  return getReceiveNeighborIDs();
}


//------------------------------------------------------------- static global holding last send face indexed
int SEND_FACE_INDEX_LAST_ACCESSED[MAX_NUM_NEIGHBORS];

FacePtr Send_Face_Begin(int procNum)
{
  int i;
  int *SendNeighborIDs = getSendNeighborIDs();
  /* find index of SendNeighborIDs which is = procNum */
  for(i=0; i< MAX_NUM_NEIGHBORS ; i++)
  {
    if( SendNeighborIDs[i] == procNum )
      break;
  }
  if( i ==MAX_NUM_NEIGHBORS )
    printf("  ERROR in Send_Face_Begin, procNum=%d not found in SendNeighborIDs.\n",procNum);
  
  SEND_FACE_INDEX_LAST_ACCESSED[i] = 0;
  return (getSendFaces(procNum)[0]);
}

FacePtr Send_Face_End(int procNum)
{
  return NULL;
}

FacePtr Next_Send_Face(int procNum)
{
  int i;
  int *SendNeighborIDs = getSendNeighborIDs();
  
  /* find index of SendNeighborIDs which is = procNum */
  for(i=0; i< MAX_NUM_NEIGHBORS ; i++)
  {
    if( SendNeighborIDs[i] == procNum )
      break;
  }
  if( i ==MAX_NUM_NEIGHBORS )
    printf("  ERROR in Next_Send_Face, procNum=%d not found in SendNeighborIDs.\n",procNum);
  
  return (getSendFaces(procNum)[ ++SEND_FACE_INDEX_LAST_ACCESSED[i] ]);
}

int RECEIVE_FACE_INDEX_LAST_ACCESSED[MAX_NUM_NEIGHBORS];

FacePtr Receive_Face_Begin(int procNum)
{
  int i;
  int *RecvNeighborIDs = getReceiveNeighborIDs();
  
  /* find index of RecvNeighborIDs which is = procNum */
  for(i=0; i< MAX_NUM_NEIGHBORS ; i++)
  {
    if( RecvNeighborIDs[i] == procNum )
      break;
  }
  if( i ==MAX_NUM_NEIGHBORS )
    printf("  ERROR in Receive_Face_Begin, procNum=%d not found in SendNeighborIDs.\n",procNum);
  
  
  RECEIVE_FACE_INDEX_LAST_ACCESSED[i] = 0;
  return (getReceiveFaces(procNum)[0]);
}

FacePtr Receive_Face_End(int procNum)
{
  return NULL;
}

FacePtr Next_Receive_Face(int procNum)
{
  int i;
  int *RecvNeighborIDs = getReceiveNeighborIDs();
  
  /* find index of RecvNeighborIDs which is = procNum */
  for(i=0; i< MAX_NUM_NEIGHBORS ; i++)
  {
    if( RecvNeighborIDs[i] == procNum )
      break;
  }
  if( i ==MAX_NUM_NEIGHBORS )
    printf("  ERROR in Next_Receive_Face, procNum=%d not found in SendNeighborIDs.\n",procNum);
  
  
  return (getReceiveFaces(procNum)[ ++RECEIVE_FACE_INDEX_LAST_ACCESSED[i] ]);
}
