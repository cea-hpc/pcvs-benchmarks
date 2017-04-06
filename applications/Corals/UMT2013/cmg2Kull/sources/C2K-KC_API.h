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
#ifndef C2KKCAPI
#define C2KKCAPI

#include <stdio.h>
#include <stdlib.h>

#include "C2K-Storage.h"

//======================================== NODE related functions
int			Number_Of_Owned_Nodes ( );
int			Local_NodeID_From_Ptr (NodePtr pN);
int			Global_NodeID_From_Ptr (NodePtr pN);

NodePtr		Owned_Node_Begin ( );
NodePtr		Owned_Node_End   ( );
NodePtr		Next_Owned_Node  ( );

//======================================== EDGE related functions
int			Number_Of_Owned_Edges ( );
int			Local_EdgeID_From_Ptr (EdgePtr pN);

EdgePtr		Owned_Edge_Begin ( );
EdgePtr		Owned_Edge_End   ( );
EdgePtr		Next_Owned_Edge  ( );

//======================================== ZONE related functions
int			Number_Of_Owned_Zones ( );
int			Local_ZoneID_From_Ptr (ZonePtr pZ);

ZonePtr		Owned_Zone_Begin ( );
ZonePtr		Owned_Zone_End   ( );
ZonePtr		Next_Owned_Zone  ( );

CornerPtr	Zones_Corner_Begin (ZonePtr pZ);
CornerPtr	Zones_Corner_End   (ZonePtr pZ);
CornerPtr	Zones_Next_Corner  (          );

SidePtr		Zones_Side_Begin (ZonePtr pZ);
SidePtr		Zones_Side_End   (ZonePtr pZ );
SidePtr		Zones_Next_Side  ( );

FacePtr		Zones_Face_Begin (ZonePtr pZ);
FacePtr		Zones_Face_End   (ZonePtr pZ);
FacePtr		Zones_Next_Face  ( );

//======================================== FACE related functions
int			Number_Of_Owned_Faces ( );
int			Local_FaceID_From_Ptr (FacePtr pF);
int			Global_FaceID_From_Ptr (FacePtr pF);

FacePtr		Owned_Face_Begin ( );
FacePtr		Owned_Face_End   ( );
FacePtr		Next_Owned_Face  ( );

CornerPtr	Faces_Corner_Begin (FacePtr pF);
CornerPtr	Faces_Corner_End   (FacePtr pF);
CornerPtr	Faces_Next_Corner  (          );

SidePtr	Faces_Side_Begin (FacePtr pF);
SidePtr	Faces_Side_End   (FacePtr pF);
SidePtr	Faces_Next_Side  (          );

FacePtr		Faces_Opposite_Face (FacePtr pF);
char*     Get_Boundary_Face_Tag(FacePtr pF);

//======================================== SIDE related functions
int			Number_Of_Owned_Sides ( );
int			Local_SideID_From_Ptr (SidePtr pS);

SidePtr		Owned_Side_Begin ( );
SidePtr		Owned_Side_End   ( );
SidePtr		Next_Owned_Side  ( );

ZonePtr   Sides_Zone          (SidePtr pS);
FacePtr		Sides_Face          (SidePtr pS);
CornerPtr	Sides_Right_Corner  (SidePtr pS);
CornerPtr	Sides_Left_Corner   (SidePtr pS);
SidePtr		Sides_Opposite_Side (SidePtr pS);

//======================================== CORNER related functions
int			Number_Of_Owned_Corners ( );
int			Local_CornerID_From_Ptr (CornerPtr pC);

CornerPtr	Owned_Corner_Begin ( );
CornerPtr	Owned_Corner_End   ( );
CornerPtr	Next_Owned_Corner  ( );

NodePtr		Corners_Node (CornerPtr pC);
ZonePtr		Corners_Zone (CornerPtr pC);

FacePtr		Corners_Face_Begin (CornerPtr pC);
FacePtr		Corners_Face_End   (CornerPtr pC);
FacePtr		Corners_Next_Face  (            );

//======================================== FACE communication functions
int     Number_Of_Send_Neighbors( );
int     Number_Of_Receive_Neighbors( );

int*      Send_Neighbor_IDs( );

FacePtr   Send_Face_Begin( int domainID );
FacePtr   Send_Face_End( int domainID  );
FacePtr   Next_Send_Face( int domainID  );

int*      Receive_Neighbor_IDs( );

FacePtr   Receive_Face_Begin( int domainID  );
FacePtr   Receive_Face_End( int domainID  );
FacePtr   Next_Receive_Face( int domainID  );

#endif
