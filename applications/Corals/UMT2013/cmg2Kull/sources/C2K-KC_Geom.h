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
#ifndef C2KKCGEOM
#define C2KKCGEOM

#include "C2K-Storage.h"

//-------------------- Prototypes for functions

void CalcFaceCentroidFromNodes  (FacePtr pF);
void CalcCentroidsOfAZonesFaces (ZonePtr pZ);
void CalcZoneCentroidFromFaces	(ZonePtr pZ);

void CalcAllFaceCentroids ();
void CalcAllZoneCentroids ();

double CalcTriangleArea (Loc3DTD P1, Loc3DTD P2, Loc3DTD P3);
double CalcFaceArea (FacePtr pFace);

double CalcTetVol  (Loc3DTD P1, Loc3DTD P2, Loc3DTD P3, Loc3DTD P4);
double CalcZoneVol (ZonePtr pZ);

double CalculateAllZoneVolumes ( int PrintFlag );

#endif

