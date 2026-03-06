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
#ifdef __APPLE__
#include <Math.h>
#else
#include <math.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include "C2K-Storage.h"
#include "C2K-KC_Geom.h"
#include "C2K-Lists.h"


void CalcFaceCentroidFromNodes (FacePtr pF)
{
	double		Cx = 0.0, Cy = 0.0, Cz = 0.0;	// to sum x, y, and z locations for all Nodes
	int			numNodes, nN;					// to count and loop over Nodes
	LItemPtr	pNLI;							// the Node list of the Face
	NodePtr		pN;

	pNLI = pF->p1stNodeLI;
	numNodes = CountListItems (pNLI);
	
	for (nN = 0; nN < numNodes; nN++)
	{
		pN = (NodePtr)RetrieveListItem (pNLI, umNODE, nN);
		Cx += pN->Loc3D.x;
		Cy += pN->Loc3D.y;
		Cz += pN->Loc3D.z;
	}
	pF->Centroid.x = Cx / numNodes;
	pF->Centroid.y = Cy / numNodes;
	pF->Centroid.z = Cz / numNodes;
}

void CalcZoneCentroidFromFaces (ZonePtr pZ)
{
	double	Cx, Cy, Cz;
	int		numFaces;
	FacePtr	pF;
	
	numFaces = 0; Cx = 0.0; Cy = 0.0; Cz = 0.0; pF = pZ->p1stFace;
	
	while (pF != NULL)
	{
		numFaces = numFaces + 1;
		Cx += pF->Centroid.x;
		Cy += pF->Centroid.y;
		Cz += pF->Centroid.z;
		
		pF = pF->pZsNextF;
	}
	pZ->Centroid.x = Cx / numFaces;
	pZ->Centroid.y = Cy / numFaces;
	pZ->Centroid.z = Cz / numFaces;
}

void CalcCentroidsOfAZonesFaces (ZonePtr pZ)
{	FacePtr	pF;
	
	pF = pZ->p1stFace;
	while (pF != NULL)
	{	CalcFaceCentroidFromNodes ( pF );
		pF = pF->pZsNextF;
	}
}

void CalcAllFaceCentroids ( )
{	FacePtr		pF;
	
	pF = pFace (0); //----- Starting with the first "real" Face
	while (pF != NULL) //-------------- loop over all Faces
	{
		CalcFaceCentroidFromNodes ( pF ); // Update the Face centroid
		pF = pF->pNextFace;
	}
}

void CalcAllZoneCentroids ( )
{	ZonePtr		pZ;
	
	pZ = pZone (0); //----- Starting with the first "real" Zone
	while (pZ != NULL) //-------------- loop over all Zones
	{
		CalcZoneCentroidFromFaces ( pZ ); // Update the Face centroid
		pZ = pZ->pNextZone;
	}
}

double CalcTriangleArea (Loc3DTD P1, Loc3DTD P2, Loc3DTD P3)
{
	double		Ux, Uy, Uz, Vx, Vy, Vz, dX, dY, dZ; // two initial vectors and cross product
	
	Ux = P2.x - P1.x;	Uy = P2.y - P1.y;	Uz = P2.z - P1.z;
	Vx = P3.x - P1.x;	Vy = P3.y - P1.y;	Vz = P3.z - P1.z;
	
	dX = Uy*Vz-Uz*Vy;	dY = Uz*Vx-Ux*Vz;	dZ = Ux*Vy-Uy*Vx;
	
	return ( 0.5 * sqrt(dX*dX + dY*dY + dZ*dZ) );
}

double CalcFaceArea (FacePtr pFace)
{	SidePtr		pS;		// to loop over a Face's Sides
	double		Area;	// Face area
	Loc3DTD		FC;		// Face centroid
	
	CalcFaceCentroidFromNodes ( pFace );
	FC   = pFace->Centroid;
	Area = 0.0;
	
	pS = pFace->p1stSide;
	while (pS != NULL)
	{	
		Area = Area + CalcTriangleArea (FC, pS->pNode1->Loc3D, pS->pNode2->Loc3D);
		pS = pS->pFsNextS;
	}
	return (Area);
}

double CalcTetVol (Loc3DTD P1, Loc3DTD P2, Loc3DTD P3, Loc3DTD P4)
{	//----- calculates the volume of a tetrahedron,
	//		assuming P1,P2,P3 are CCW wrt centroid of tet
	//		Usually, P1=Node1, P2=Node2, P3=Face Centroid, P4=Zone Centroid
	
	double Ax, Ay, Az, Bx, By, Bz, Cx, Cy, Cz, Volume;
	
	Ax = P4.x - P3.x;  Ay = P4.y - P3.y;  Az = P4.z - P3.z;
	Bx = P1.x - P3.x;  By = P1.y - P3.y;  Bz = P1.z - P3.z;
	Cx = P2.x - P3.x;  Cy = P2.y - P3.y;  Cz = P2.z - P3.z;
	
	Volume = (Ax * (By*Cz - Bz*Cy)
		   +  Ay * (Bz*Cx - Bx*Cz)
		   +  Az * (Bx*Cy - By*Cx))/6.0;
	return (Volume);
}

double CalcZoneVol (ZonePtr pZ)
{
	SidePtr		pS;
	double		Volume;
	Loc3DTD		ZoneCentroid;
	
	ZoneCentroid = pZ->Centroid;
	Volume = 0.0;
	pS = pZ->p1stSide;
	while (pS != NULL)
	{
		Volume = Volume + CalcTetVol(pS->pNode1->Loc3D,
									 pS->pNode2->Loc3D,
									 pS->pFace->Centroid,
									 ZoneCentroid);
		pS= pS->pZsNextS;
	}
	return (Volume);
}

double CalculateAllZoneVolumes ( int PrintFlag )// 0 means no printing
{												// 1 means just print the cumulative total
	double		TotalVolume, ZoneVolume;		// 2 means print all zones and cumulative
	ZonePtr		pZ;
	int			c2k_numZones;

	if ( PrintFlag == 2 ) printf("Volume by Zone, and cummulative:\n");
	TotalVolume = 0.0;
	c2k_numZones    = 0;
	pZ = pZone (0);
	while (pZ != NULL)
	{	c2k_numZones += 1;
		ZoneVolume = CalcZoneVol ( pZ );
		TotalVolume += ZoneVolume;

		if ( PrintFlag == 2)
			printf("Zone # = %3d, Zone Volume = %15.12f, Total Volume = %15.12f \n", 
					c2k_numZones,	  ZoneVolume,		 TotalVolume);
		pZ = pZ->pNextZone;
	}
	if ( PrintFlag == 1) printf("Total volume = %15.12f\n", TotalVolume);
	return (TotalVolume);
}
