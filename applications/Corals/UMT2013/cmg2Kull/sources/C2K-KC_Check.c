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
#include "C2K-KC_Info.h"


int CheckFaceInfo ( FacePtr pF, char  *s )
{	int			numSides, numNodes, numMaxSN, numErrors;
	SidePtr		pS, pSLast, pSideAtFirstNode, pBadSide;
	LItemPtr	pNLI;
	NodePtr		pN2Check;

	numErrors = 0;
	pBadSide  = NULL;
	//------------------------------ Compare the number of Nodes and Sides
	numMaxSN  = 0;
	numSides = NumSidesInFace(pF);
	numNodes = NumNodesOnFace(pF);

	if ( numSides != numNodes )
	{	printf("\n\n============================================================\n");
		printf("\n*****\n");
		printf("ERROR; # of Sides != # of Nodes on Face.\n");
		printf("# of Sides = %2d\n", numSides);
		printf("# of Nodes = %2d\n", numNodes);
		printf("*****\n");
		
		if (numSides > numNodes)
			numMaxSN = numSides;
		else
			numMaxSN = numNodes;
		numErrors = 1;
	}
	//------------------------------ Check if Face's Side list is CCW
	pS = pF->p1stSide;
	while (pS != NULL)
	{	pSLast = pS;

		pS= pS->pFsNextS;
		if (pS != NULL)
		{	
			if (pS != pSLast->pLeftSide)
			{	if (numErrors == 0) printf("\n\n============================================================\n");
				printf("\n*****\n");
				printf (" ERROR: Next Side in Face's CCW side list is not LeftSide.\n");
				printf("*****\n");
				pBadSide  = pSLast;
				numErrors = numErrors + 1;
			}
			if (pS->pNode1 != pSLast->pNode2)
			{	if (numErrors == 0) printf("\n\n============================================================\n");
				printf("\n*****\n");
				printf (" ERROR: Node 1 of next Side in Face's CCW side list != Node 2 of current Side.\n");
				printf("*****\n");
				pBadSide  = pSLast;
				numErrors = numErrors + 1;
			}
		}
	}
	//------------------------------ Check if Face's Node list is CCW
	pSideAtFirstNode = NULL;
	pNLI = pF->p1stNodeLI;
	SetupSeqListAccess	 (pNLI, umNODE);
	pN2Check = (NodePtr)GetNextEntityFromList (pNLI, umNODE);
	pS	 = pF->p1stSide;
	while (pS != NULL) //----- first, search for side with Node 1 = first node in CCW Node list
	{	
		if (pS->pNode1 == pN2Check) //----- found
		{	pSideAtFirstNode = pS;
			break;
		}
		pS = pS->pFsNextS;
	}
	if (pSideAtFirstNode == NULL) //----- Side was NOT found
	{	if (numErrors == 0) printf("\n\n============================================================\n");
		printf("\n*****\n");
		printf (" ERROR: Face's 1st Node was not found among Sides' Nodes.\n");
		printf("*****\n");
		numErrors = numErrors + 1;
	}
	else if ( pBadSide != NULL ) //----- Side was found, but Face's side list was not CCW
	{
		printf("\n*****\n");
		printf (" ERROR: Can't check that Node list is CCW, until Side list is CCW.\n");
		printf("*****\n");
	}
	else //----- OK to proceed with CCW check on Nodes, which relies on CCW side list
	{
		pS	 = pSideAtFirstNode;
		while (pN2Check != NULL) //----- loop over all Nodes
		{	
			if (pS->pNode1 != pN2Check) //----- Node order does not follow CCW Sides
			{	if (numErrors == 0) printf("\n\n============================================================\n");
				printf("\n*****\n");
				printf (" ERROR: Node order does not follow CCW Sides.\n");
				printf("*****\n");
				numErrors = numErrors + 1;
			}
			pS	 = pS->pLeftSide;
			pN2Check = (NodePtr)GetNextEntityFromList (pNLI, umNODE);;
		}
	}
	
	if (numErrors > 0)
	{	printf("Location of failed Face check: %s\n", s);
		printf("============================================================\n\n");
		printf("\n");
	}
	return (numErrors);
}

int CheckZoneInfo ( ZonePtr pZ, char *s )
{	
	SidePtr		pS;
	int			numErrors, numZsSides, numZsFaces, numZsFsSides;
	FacePtr		pF;
	Loc3DTD		L;		// a Node's physical location in 3D space
	NodePtr		pN;		// to report errors
	
	numErrors  = 0;
	numZsSides = 0;
	
	pS = pZ->p1stSide;
	while (pS != NULL)
	{	//----- count the sides registered with this zone
		numZsSides	  = numZsSides + 1;
		//-----  check if the Side points to this Zone
		if (pS->pZone != pZ)
		{	numErrors = numErrors + 1;
			printf("\n\n*****\nERROR; Side points to wrong Zone\n*****\n");
		}
		//----- now check that each side has been placed into the side list of both its Nodes
		
		if ( FindEntityInList(pS->pNode1->p1stSideLI, umSIDE, pS) < 0)
		{	numErrors = numErrors + 1;
			printf("\n\n*****\nERROR; Side not registered at Node1\n*****\n");
			pN = pS->pNode1;
			printf("Node LID = %3d\n", pN->nNodeLID);
			L  = pN->Loc3D;
			printf("x = %e, y = %e, z = %e\n", L.x, L.y, L.z);
			
		}
		if ( FindEntityInList(pS->pNode2->p1stSideLI, umSIDE, pS) < 0)
		{	numErrors = numErrors + 1;
			printf("\n\n*****\nERROR; Side not registered at Node2\n*****\n");
			pN = pS->pNode2;
			printf("Node LID = %3d\n", pN->nNodeLID);
			L  = pN->Loc3D;
			printf("x = %e, y = %e, z = %e\n", L.x, L.y, L.z);
		}
		pS= pS->pZsNextS;
	}

	numZsFaces   = 0;
	numZsFsSides = 0;
	
	pF = pZ->p1stFace;
	while (pF != NULL)
	{	numZsFaces = numZsFaces + 1;
		
		numErrors = numErrors + CheckFaceInfo ( pF, " called from CheckZoneInfo.\n" );
		
		pS = pF->p1stSide;
		while (pS != NULL)
		{	numZsFsSides = numZsFsSides + 1;
			if (pS->pZone != pZ)
			{	numErrors = numErrors + 1;
				printf("\n\n*****\nERROR; Side on Face points to wrong Zone.\n*****\n");
			}
			if (pS->pFace != pF)
			{	numErrors = numErrors + 1;
				printf("\n\n*****\nERROR; Side on Face points to wrong Face.\n*****\n");
			}
			pS= pS->pFsNextS;
		}
		pF = pF->pZsNextF;
	}
	if (numZsFsSides != numZsSides)
	{	numErrors = numErrors + 1;
		printf (" ERROR: # in Zone's Side list != sum of Sides in Face's side lists.\n");
	}
	if (numErrors > 0)
	{	printf("Location of failed Zone check: %s\n", s);
		printf("============================================================\n\n");
		printf(" \n");
	}
	return (numErrors);
}

int  CheckEdgeInfo ( EdgePtr pE, char *s )
{	SidePtr		pS1, pS2;
	int			numErrors = 0;
	int			numSides  = 0;
	
	if (pE->p1stSide == NULL)
	{	printf ("***** ERROR Edge has no Sides.\n");
		numErrors += 1;
	}
	
	pS1 = pE->p1stSide;
	while (pS1 != NULL)
	{	numSides +=1;
		pS1 = pS1->pEsNextS;
	}
	
	if (numSides % 2 != 0)
	{	printf("ERROR; Number of Sides at an Edge is Odd, #Sides = %d\n", numSides);
		numErrors += 1;
	}
	
	if (numSides == 2)
	{	pS1 = pE->p1stSide; 
		pS2 = pS1->pEsNextS;
		if ( (pS1->pOppositeSide != NULL) || (pS2->pOppositeSide != NULL) )
		{	printf ("ERROR; One Zone Edge is not on the boundary.\n");
			numErrors +=1;
		}
	}
	
	pS1 = pE->p1stSide;
	while (pS1 != NULL)
	{	pS2 = pS1->pEsNextS;
		while (pS2 != NULL)
		{	if (pS1->pFace == pS2->pFace)
			{	printf ("***** ERROR Two Sides at Edge point to the same face.\n");
				numErrors = numErrors + 1;
			}
			pS2 = pS2->pEsNextS;
		}
		pS1 = pS1->pEsNextS;
	}

	pS1 = pE->p1stSide;
	while (pS1 != NULL)
	{	
		if (pS1->pEdge != pE)
		{	printf ("***** ERROR Side in Edge's Side list doesn't point to Edge.\n");
			numErrors = numErrors + 1;
		}
		pS1 = pS1->pEsNextS;
	}
	
	if (numErrors > 0)
	{	printf("Failed Edge check: %s\n", s);
		PrintEdgeInfo ( pE );
		exit (1);
	}
	return (numErrors);
}

int  CheckAllEdgeInfo ( char *s )
{	EdgePtr		pE;
	int			numErrors = 0;
	
	pE = pEdge (0);
	while (pE != NULL)
	{	numErrors = numErrors + CheckEdgeInfo ( pE, s);
		pE = pE->pNextEdge;
	}
	if (numErrors > 0) printf ("Failed Edge check: %s \n", s);
	return (numErrors);
}

void CheckAllConnectivityInfo ( char *s )
{	ZonePtr		pZ;
	int			nSAAE, numSides = 0;
	SidePtr		pS;
	
	pZ = pZone (0);
	while (pZ != NULL)
	{
		if ( CheckZoneInfo ( pZ, s ) > 0)
		{	exit (1);
		}
		pZ = pZ->pNextZone;
	}
	if (CheckAllEdgeInfo ( s ) > 0)
	{	exit(1);
	}
	nSAAE = NumSidesAtAllEdges ( );
	pS = pSide (0);
	while (pS != NULL)
	{	numSides +=1;
		pS = pS->pNextSide;
	}
	if (numSides != nSAAE)
	{	printf("ERROR; Sum of Sides at Edges != numSides.\n");
		exit(1);
	}
}

void CACI ( char *s )
{
	CheckAllConnectivityInfo ( s );
}
