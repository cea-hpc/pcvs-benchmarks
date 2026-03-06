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
#ifndef C2KKCINFO
#define C2KKCINFO

#include "C2K-Storage.h"

//-------------------- Get information about Mesh entities, or the entire mesh

int NumNodesOnFace   (FacePtr	pF);
int NumCornersOnFace (FacePtr	pF);
int NumSidesInFace   (FacePtr	pF);
int NumZonesAtEdge   (EdgePtr	pE);
int NumNodesOfZone   (ZonePtr	pZ);
int NumFacesOfZone   (ZonePtr	pZ);
int NumCornersOfZone (ZonePtr	pZ);
int NumSidesOfZone   (ZonePtr	pZ);
int NumFacesAtCorner (CornerPtr	pC);
int NumSidesAtCorner (CornerPtr	pC);
int NumEdgesAtNode   (NodePtr	pN);
int NumFacesAtNode   (NodePtr	pN);
int NumSidesAtNode   (NodePtr	pN);
int NumberOfFacesBetweenZones (ZonePtr pZ1, ZonePtr pZ2);

int NumSidesAtAllEdges   ( );

//-------------------- Print information about Mesh entities

void PrintCountOfAllListItems ( );
void PrintZoneInfo (ZonePtr pZ);
void PrintFaceInfo (FacePtr pF);
void PrintEdgeInfo (EdgePtr pE);

void PrintFaceSideDistribution ( );

#endif
