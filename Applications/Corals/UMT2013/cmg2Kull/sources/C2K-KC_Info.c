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

int NumNodesOnFace (FacePtr pF)
{	
	return (CountListItems ( pF->p1stNodeLI ));
}

//int NumNodesOnFace (FacePtr pF)
//{	int			nN;
//	LItemPtr	pNLI;
//	
//	nN = 0;
//	pNLI = pF->p1stNodeLI;
//	while (pNLI != NULL)
//	{
//		nN = nN + 1;
//		pNLI = pNLI->pNextItem;
//	}
//	return ( nN );
//}

int NumCornersOnFace (FacePtr pF)
{	
	return (CountListItems ( pF->p1stCornerLI ));
}

int NumSidesInFace (FacePtr pF)
{	int			nS;
	SidePtr		pS;
	
	nS = 0;
	pS = pF->p1stSide;
	while (pS != NULL)
	{
		nS = nS + 1;
		pS = pS->pFsNextS;
	}
	return ( nS );
}

int NumZonesAtEdge (EdgePtr pEdge)
{	int		Count;
	SidePtr		pS;
	
	Count = 0;
	pS = pEdge->p1stSide;
	while (pS != NULL)
	{	Count = Count + 1;
		pS = pS->pEsNextS;
	}
	if (Count % 2 != 0)
	{
		printf("***** ERROR ***** Odd number of sides at Edge.");
		exit (1);
	}
	return (Count/2);
}

int NumNodesOfZone (ZonePtr pZ)
{	SidePtr   pS;
	int			  NodesInZone;
	LItemPtr	pZsNodeList;	// to collect a Zone's Nodes

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
	return (NodesInZone);
}

int NumFacesOfZone (ZonePtr pZ)
{	FacePtr		pF;
	int			numF;
	
	numF = 0;
	pF = pZ->p1stFace;
	while (pF != NULL)
	{	numF = numF + 1;
		pF = pF->pZsNextF;
	}
	return (numF);
}

int NumCornersOfZone (ZonePtr pZ)
{	CornerPtr	pC;
	int			numC = 0;
	
	pC = pZ->p1stCorner;
	while (pC != NULL)
	{	numC = numC + 1;
		pC = pC->pZsNextC;
	}
	return (numC);
}

int NumSidesOfZone (ZonePtr pZ)
{	SidePtr	pS;
	int		numS = 0;
	
	pS = pZ->p1stSide;
	while (pS != NULL)
	{	numS = numS + 1;
		pS = pS->pZsNextS;
	}
	return (numS);
}

int NumFacesAtCorner (CornerPtr pC)
{	
	return (CountListItems ( pC->p1stFaceLI ));
}

int NumSidesAtCorner (CornerPtr pC)
{	
	return (CountListItems ( pC->p1stSideLI ));
}

int NumEdgesAtNode (NodePtr pN) //----- counts the number of Edges that point to a NodeC)
{	
	return (CountListItems ( pN->p1stEdgeLI ));
}

int NumFacesAtNode (NodePtr pN) //----- counts the number of Faces that point to a NodeC)
{	
	return (CountListItems ( pN->p1stFaceLI ));
}

int NumSidesAtNode (NodePtr pN) //----- counts the number of Sides that point to a NodeC)
{	
	return (CountListItems ( pN->p1stSideLI ));
}

//int NumEdgesAtNode (NodePtr pN) //----- counts the number of Edges that point to a Node
//{	LItemPtr	pSLI;
//	LItemPtr	pEdgeList;
//	int			numE, numS, nS;
//	SidePtr		pS;
//	
//	pEdgeList = (LItemPtr)MakeList (umEDGE);	//----- Make a temporary list to hold Edge pointers
//	
//	pSLI = pN->p1stSideLI;
//	numS = CountListItems (pSLI);
//	for (nS = 0; nS < numS; nS++) //----- Loop over all Sides at a Node
//	{	
//		pS = (SidePtr)RetrieveListItem (pSLI, umSIDE, nS);
//		
//		if (FindEntityInList (pEdgeList, umEDGE, pS->pEdge) < 0)	// if Side's Edge is not in the list
//			 AddEntityToListEnd (pEdgeList, umEDGE, pS->pEdge);		// add it to the list
//	}
//	numE = CountListItems (pEdgeList);  //----- Count the number of items in the list
//	DeleteList (pEdgeList);				//----- Delete the Edge list
//	return (numE);
//}

int NumberOfFacesBetweenZones (ZonePtr pZ1, ZonePtr pZ2)
{	FacePtr		pFace;
	int			Count = 0;
	SidePtr		pSO;
	
	pFace = pZ1->p1stFace; // Starting with the first Face of a Zone
	while (pFace != NULL)
	{
		pSO = pFace->p1stSide->pOppositeSide;
		if (pSO != NULL) //---------- only check interior Faces
		{	if (pSO->pZone == pZ2) Count +=1;
		}
		pFace = pFace->pZsNextF; //---------- Move to the next Face of the Zone
	}
	return (Count);
}

int NumSidesAtAllEdges   ( )
{	EdgePtr		pE;
	SidePtr		pS;
	int			Count = 0;
	
	pE = pEdge ( 0 );
	while (pE != NULL)
	{
		pS = pE->p1stSide;
		while (pS != NULL)
		{	Count +=1;
			pS = pS->pEsNextS;
		}
		pE = pE->pNextEdge;
	}
	return (Count);
}

void PrintCountOfAllListItems ( )
{	int		i;					// all-purpose loop index
	int		numF = getNumF( );	// Total # of Faces
	int		numC = getNumC( );	// Total # of Corners
	int		numN = getNumN( );	// Total # of Nodes
	int		numLI_N_On_F = 0;	// Total # of Node List Items On Faces
	int		numLI_C_On_F = 0;	// Total # of Corner List Items On Faces
	int		numLI_F_At_C = 0;	// Total # of Face List Items At Corners
	int		numLI_S_At_C = 0;	// Total # of Side List Items At Corners
	int		numLI_E_At_N = 0;	// Total # of Edge List Items At Nodes
	int		numLI_F_At_N = 0;	// Total # of Face List Items At Nodes
	int		numLI_S_At_N = 0;	// Total # of Side List Items At Nodes
	NodePtr	pN;
	
	printf("Total Number of List Items in:\n");

	numF = getNumF ( );	printf("# Faces   = %4d\n", numF);
	numC = getNumC ( );	printf("# Corners = %4d\n", numC);
	numN = getNumN ( );	printf("# Nodes   = %4d\n", numN);
	
	for (i = 0; i < numF; i++)
	{	numLI_N_On_F += NumNodesOnFace   ( pFace(i) );
		numLI_C_On_F += NumCornersOnFace ( pFace(i) );
	}
	printf("# Node   LIs on Faces   = %4d\n", numLI_N_On_F);
	printf("# Corner LIs on Faces   = %4d\n", numLI_C_On_F);
	
	for (i = 0; i < numC; i++)
	{	numLI_F_At_C += NumFacesAtCorner ( pCorner(i) );
		numLI_S_At_C += NumSidesAtCorner ( pCorner(i) );
	}
	printf("# Face   LIs at Corners = %4d\n", numLI_F_At_C);
	printf("# Side   LIs at Corners = %4d\n", numLI_S_At_C);
	
	for (i = 0; i < numN; i++)
	{	pN = pNode(i);
		printf("Node # %3d, LID = %3d\n", i, pN->nNodeLID);
		
		numLI_E_At_N += NumEdgesAtNode ( pN );	printf("Edges @ Node, N# %3d, running sum = %3d\n", i, numLI_E_At_N);
		numLI_F_At_N += NumFacesAtNode ( pN );	printf("Faces @ Node, N# %3d, running sum = %3d\n", i, numLI_F_At_N);
		numLI_S_At_N += NumSidesAtNode ( pN );	printf("Sides @ Node, N# %3d, running sum = %3d\n", i, numLI_S_At_N);
	}
	
	printf("Node Lists of Faces   = %4d\n", numLI_N_On_F);
	printf("Corner Lists of Faces = %4d\n", numLI_C_On_F);
	printf("Face Lists at Corners = %4d\n", numLI_F_At_C);
	printf("Side Lists at Corners = %4d\n", numLI_S_At_C);
	printf("Edge Lists at Nodes   = %4d\n", numLI_E_At_N);
	printf("Face Lists at Nodes   = %4d\n", numLI_F_At_N);
	printf("Side Lists at Nodes   = %4d\n", numLI_S_At_N);
}


void PrintFaceInfo ( FacePtr pF) // For debugging
{	SidePtr		pS;
	int			Count = -1;
	
//	printf ("\nCentroid, x = %3e, y = %3e, z = %3e\n", pF->Centroid.x, pF->Centroid.y, pF->Centroid.z);
	printf ("# Nodes = %3d, # Sides = %3d\n\n", NumNodesOnFace (pF), NumSidesInFace (pF));
	printf ("| Side#  |  SideID   |  LeftID   |  RightID  |   EdgeID  |\n");
	
	pS = pF->p1stSide;
	while (pS != NULL)
	{	Count = Count + 1;
		printf ("| %4d   |  %p  |  %p  |  %p  |  %p  |\n", Count, pS, pS->pLeftSide, pS->pRightSide, pS->pEdge);
		
		pS = pS->pFsNextS;
	}
}

void PrintEdgeInfo (EdgePtr pEdge) // For debugging
{	SidePtr		pS;
	int			Count = -1;
	Loc3DTD		Loc3D;
	
	printf ("\nEdge's location = %p\n", pEdge);
	Loc3D = pEdge->pNode1->Loc3D;
	printf ("Node1:[%p]  X = %3e, Y = %3e, Z = %3e\n", pEdge->pNode1, Loc3D.x, Loc3D.y, Loc3D.z);
	Loc3D = pEdge->pNode2->Loc3D;
	printf ("Node2:[%p]  X = %3e, Y = %3e, Z = %3e\n", pEdge->pNode2, Loc3D.x, Loc3D.y, Loc3D.z);

	printf ("| Side#  |  SideID   |  FaceID   |  ZoneID |\n");
	
	pS = pEdge->p1stSide;
	while (pS != NULL)
	{	Count = Count + 1;
		printf ("| %4d   |  %p  |  %p  |  %p  |\n", Count, pS, pS->pFace, pS->pZone);
		
		pS = pS->pEsNextS;
	}
}



int		PFSD_Header = 1; //----- State variable to control the header for PrintFaceSideDistribution

void TogglePrintFSD_Header ( )
{	PFSD_Header = 0;
}

void PrintSideInfoHeader (  )
{
	printf (" Side#  FC.x   FC.y   FC.z   N1.x   N1.y   N1.z   N2.x   N2.y   N2.z   Vol  \n");
}

void PrintSideInfo (int numSide, Loc3DTD FC, Loc3DTD N1, Loc3DTD N2, double vol)
{
	printf ("  %3d  %5.2f, %5.2f, %5.2f",numSide, FC.x, FC.y, FC.z );
	printf (     "  %5.2f, %5.2f, %5.2f",         N1.x, N1.y, N1.z );
	printf (     "  %5.2f, %5.2f, %5.2f  %8.5f\n",N2.x, N2.y, N2.z, vol );
}

void PrintSideInfoHeader2 (  ) // remove Face centroid and add precision
{
	printf (" Side#  N1.x       N1.y       N1.z     ||   N2.x       N2.y       N2.z       Vol  \n");
}

void PrintSideInfo2 (int numSide, Loc3DTD N1, Loc3DTD N2, double vol)
{
	printf ("  %3d",numSide );
	printf (     "  %9.6f, %9.6f, %9.6f ||",           N1.x, N1.y, N1.z      );
	printf (     "  %9.6f, %9.6f, %9.6f  %13.10f\n",N2.x, N2.y, N2.z, vol );
}

void PrintZoneInfo ( ZonePtr pZ )
{	
	SidePtr		pS, pSLast;
	double		TetVolume, TotalVolume;
	Loc3DTD		ZoneCentroid, FaceC, N1L, N2L;
	int			numSide, numFace;
	FacePtr		pF;
	
	ZoneCentroid = pZ->Centroid;
	printf("\n----------\nInfo for Zone centered at [%e, %e, %e]\n----------\n",
		ZoneCentroid.x,ZoneCentroid.y,ZoneCentroid.z); 
	printf("The Zone's SIDE list:\n");
	PrintSideInfoHeader ( );
	numSide = 0;
	TotalVolume = 0.0;
	pS = pZ->p1stSide;
	while (pS != NULL)
	{	if (pS->pZone != pZ) printf("\n\n*****\nERROR; next side points to wrong zone\n*****\n");
		numSide	  = numSide + 1;
		FaceC	  = pS->pFace->Centroid;
		N1L		  = pS->pNode1->Loc3D;
		N2L		  = pS->pNode2->Loc3D;
		TetVolume =  CalcTetVol ( N1L, N2L, FaceC,ZoneCentroid);
		TotalVolume  = TotalVolume + TetVolume;
		
		PrintSideInfo (numSide, FaceC, N1L, N2L, TetVolume);
		pS= pS->pZsNextS;
	}
	printf("TOTAL VOLUME from Sides = %5.2f\n", TotalVolume);
	printf("The Zone's FACES, and their CCW ordered SIDE lists:\n");
	numFace = 0;
	pF = pZ->p1stFace;
	while (pF != NULL)
	{	numFace = numFace + 1;
		FaceC = pF->Centroid;
		printf("***** Face #= %2d [%e, %e, %e]", numFace, FaceC.x,FaceC.y,FaceC.z);
		printf(" pF= %p\n", pF);
		PrintSideInfoHeader2 ( );
		numSide = 0;
		pS = pF->p1stSide;
		while (pS != NULL)
		{	if (pS->pZone != pZ) printf("\n\n*****\nERROR; next side points to wrong zone\n*****\n");
			pSLast		 = pS;
			numSide		 = numSide + 1;
			FaceC		 = pS->pFace->Centroid;
			N1L			 = pS->pNode1->Loc3D;
			N2L			 = pS->pNode2->Loc3D;
			TetVolume    =  CalcTetVol ( N1L, N2L, FaceC,ZoneCentroid);
		
			PrintSideInfo2 (numSide, N1L, N2L, TetVolume);
			pS= pS->pFsNextS;
			if (pS == NULL)
				printf ("--------------------------------------------------\n");
			else
			{	if (pS != pSLast->pLeftSide)
					printf (" ERROR: Next Side in Face's CCW side list is not LeftSide!!\n");
			}
		}
		pF = pF->pZsNextF;
	}
}

void PrintFaceSideDistribution ( ) //----- bins the number of Sides in a Face for all Faces
#define nSMax 20
{	FacePtr		pF;
	int			nS, nSHigh, nSLow, nF,
				nSDist[nSMax];
	
	for (nS = 0; nS < nSMax; nS++) nSDist[nS] = 0;	// clear the distribution array
	nSHigh	= 0;
	nSLow	= 0;
	nF		= 0;
	
	pF = pFace (0);
	while (pF != NULL)
	{	nF = nF + 1;
		nS = NumSidesInFace(pF);
		if (nS < 3)
			nSLow = nSLow + 1;
		else if (nS > nSMax)
			nSHigh = nSHigh + 1;
		else
			nSDist[nS] = nSDist[nS] + 1;
		pF = pF->pNextFace;
	}
	if ( PFSD_Header == 0)
		printf(" #Faces    3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19\n");
	PFSD_Header = 1;
	printf(" %4d   ",nF);
	for (nS = 3; nS < nSMax; nS++)
	{
		if (nSDist[nS] > 0)
			printf(" %3d",nSDist[nS]);
		else
			printf("    ");
	}
	if (nSHigh > 0) printf(" #High=%d", nSHigh);
	if (nSLow  > 0) printf(" #Low=%d" , nSLow );
	printf("\n");
}
