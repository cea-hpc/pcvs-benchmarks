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
// All of the unstructured mesh storage management functions
#define NUM_NEW_STRUCTS 200

#include <stdio.h>
#include <stdlib.h>
#include "C2K-Storage.h"
#include "C2K-KC_Alter.h"

int	nextUniqueZID = 1;	// to give every Zone a unique ID number

int	DebugPrint = 0;		// > 0 will print whenever new objects are created

//======================================== LIST INSERTION, DELETION, ETC...


void InsertSideInFacesOrderedSideList (	SidePtr pNewSide,	// the Side to be inserted
										FacePtr pFace,		// the Face that gains the Side
							/* after */ SidePtr pSideAfter ) // after this Side to maintain CCW order
{
	SidePtr		pSide, pLastSide;
	
	pSide = pFace->p1stSide;
	while ( pSide != NULL ) //-------------------- loop over all Sides
	{
		if ( pSide == pSideAfter ) //----- Side was found after which the new Side will be inserted
		{
			pNewSide->pFsNextS = pSide->pFsNextS;	// new Side will point to next Side
			pSide->pFsNextS = pNewSide;				// the "after" side will point to the new Side
			pLastSide = NULL;						// a flag to signal the insertion has been done
			break;
		}					// NOTE Could also set left and right pointers here instead of outside this routine
		pLastSide = pSide;			// signals side not found
		pSide = pSide->pFsNextS;
	}
	if (pLastSide != NULL )
	{
		printf ("\nERROR, SIDE NOT FOUND IN LIST (InsertSideInFacesOrderedSideList)\n");
//		printf ("\nFace cent. location, x = %3f, y = %3f, z = %3f\n",pFace->Centroid.x,
//																	 pFace->Centroid.y,
//																	 pFace->Centroid.z   );
		exit (1);
	}
}

void InsertSideInEdgesSideList (SidePtr pSide, EdgePtr pEdge ) // this list is UNORDERED
{	
	if (pSide->pEdge != pEdge)
	{	printf("ERROR; Side doesn't point to Edge in InsertSideInEdgesSideList.\n");
		exit(1);
	}
	pSide->pEsNextS = pEdge->p1stSide;
	pEdge->p1stSide = pSide;
}

void RemoveSideInEdgesSideList (SidePtr pSide, EdgePtr pEdge )
{	SidePtr		pS;
	//-------------------- ERROR checking
	if (pSide == NULL)
	{	printf("ERROR; pointer to Side is NULL in RemoveSideInEdgesSideList\n");
		exit (1);
	}
	if (pEdge == NULL)
	{	printf("ERROR; pointer to Edge is NULL in RemoveSideInEdgesSideList\n");
		exit (1);
	}
	if (pSide->pEdge != pEdge)
	{	printf("ERROR; Side does not point to Edge in RemoveSideInEdgesSideList.\n");
		exit (1);
	}
	if (pSide == pEdge->p1stSide)
	{	pEdge->p1stSide = pSide->pEsNextS;	// Side is removed from list, but not itself deleted
//		if (pEdge->p1stSide == NULL)
//		{	printf("ERROR?  No more Sides left at Edge.");// Can't do this because it is always true in TryToRemoveAnEdge
//			exit(1);
//		}
	}
	else
	{	pS = pEdge->p1stSide;
		while (pS != NULL)
		{
			if (pS->pEsNextS == pSide)
			{
				pS->pEsNextS = pSide->pEsNextS;		// Side is removed from list, ...
				return;
			}
			
			pS = pS->pEsNextS;
		}
		printf ("ERROR; Side points to Edge, but not found in Edge's Side list\n");
		exit (1);
	}
}

void DeleteSidesCCWInFace (FacePtr pFace, /* BETWEEN */ SidePtr pSide1, /* and */ SidePtr pSide2)
{
	int			numSide1, numSide2, nS;
	SidePtr		pS;
	
	numSide1 = -1;			// initialize list location indices
	numSide2 = -1;
	pS = pFace->p1stSide;
	nS = 0;
	while (pS != NULL ) //---------- find the list indices of the deletion limits
	{	if		( pS == pSide1 ) {numSide1 = nS;}
		else if ( pS == pSide2 ) {numSide2 = nS;}
		
		pS = pS->pFsNextS;
		nS = nS + 1;
	}
	
	if ( (numSide1 < 0) || (numSide2 < 0) )
	{
		//----------TO DO: ERROR; one or both Sides NOT in list
	}
	
	if ( numSide1 < numSide2 ) //----- deleted Sides are in order
	{
		pSide1->pFsNextS = pSide2; // deletes zero or more Sides from the list
	}
	else if ( numSide1 > numSide2 ) //----- deleted Sides wrap around
	{
		pFace->p1stSide = pSide2;  // these 2 lines delete zero or more Sides from the list
		pSide1->pFsNextS = NULL;
	}
	//----- else, nothing to do
}

void  RemoveSideFromItsNodes ( SidePtr pSide )
{	
	RemoveEntityFromList (&(pSide->pNode1->p1stSideLI), umSIDE, pSide);
	RemoveEntityFromList (&(pSide->pNode2->p1stSideLI), umSIDE, pSide);
}

SidePtr FindSideOnFace ( FacePtr pFace, NodePtr pNode1)
{
	SidePtr		pS;
	
	pS = pFace->p1stSide;
	while ( pS != NULL )
	{
		if ( pS->pNode1 == pNode1 ) return (pS); //----- Side is found
		
		pS = pS->pFsNextS;
	}

	return ( pS ); //----- value is NULL; Side NOT found
}

void AddFaceToZonesFaceList ( FacePtr pFace, ZonePtr pZone )
{
	pFace->pZsNextF = pZone->p1stFace;
	pZone->p1stFace = pFace;
}

void RemoveFaceFromZonesFaceList ( FacePtr pFace, ZonePtr pZone )
{	FacePtr		pF;
	
	if (pFace == pZone->p1stFace)			// Face is the first in the Zone's Face list
		pZone->p1stFace = pFace->pZsNextF;	// Face is removed from list, but not itself deleted
	else
	{	pF = pZone->p1stFace;
		while ( pF->pZsNextF != pFace) //----- NOT PROTECTED FROM NOT FOUND; I.E., NULL !!
		{	pF = pF->pZsNextF;
		}
		pF->pZsNextF = pFace->pZsNextF;		// Face is removed from list, ...
	}
}

void AddSideToZonesSideList ( SidePtr pSide, ZonePtr pZone )
{
	pSide->pZsNextS = pZone->p1stSide;
	pZone->p1stSide = pSide;
}

void RemoveSideFromZonesSideList ( SidePtr pSide, ZonePtr pZone )
{	SidePtr		pS;
	
	if (pSide == pZone->p1stSide)			// Side is the first in the Zone's side list
		pZone->p1stSide = pSide->pZsNextS;	// Side is removed from list, but not itself deleted
	else
	{	pS = pZone->p1stSide;
		while ( pS->pZsNextS != pSide) //----- NOT PROTECTED FROM NOT FOUND; I.E., NULL !!
		{	pS = pS->pZsNextS;
		}
		pS->pZsNextS = pSide->pZsNextS;		// Side is removed from list, ...
	}
}

void ClearZonesFaceList ( ZonePtr pZone )
{
	pZone->p1stFace = NULL; // could also unlink Faces, but probably not necessary
}

void ClearZonesSideList ( ZonePtr pZone )
{
	pZone->p1stSide = NULL; // could also unlink Sides, but probably not necessary
}

LItemPtr CreateListOfAZonesNodes ( ZonePtr pZ )
{	FacePtr		pF;		// pointer for loop over all Faces of the Zone
	NodePtr		pN;		// pointer to a node of face that will be saved,
						// if it has not already been saved
	LItemPtr	pNLIFace;	// Node List Item pointer to loop over a Face's Nodes
	int			nFound;		// flag to indicate if a node has already been saved
	LItemPtr	pZs1stNLI;	// Pointer to the Zone's first Node list item, the RETURN VALUE
	int			nN, numFsN;	// loop index
	
	pZs1stNLI = NULL;  //----- initialize the list of unique Nodes to be empty
	
	pF = pZ->p1stFace;
	while (pF != NULL) //---------- loop over all Faces of a Zone
	{
		pNLIFace = pF->p1stNodeLI;
		numFsN = CountListItems ( pNLIFace );
		
		for (nN = 0; nN < numFsN; nN++)  //---------- loop over all of the Face's Node List Items
		{	
			pN = (NodePtr)RetrieveListItem(pNLIFace, umNODE, nN);
			
			nFound = FindEntityInList (pZs1stNLI, umNODE, pN);
			if ( nFound < 0 )									// Node not found,
				AddEntityToListEnd (pZs1stNLI, umNODE, pN);			// so add it to the Zone's Node list.
		}
		pF = pF->pZsNextF; //----- advance to the Zone's next Face
	}
	return (pZs1stNLI);
} //----- End of CreateListOfAZonesNodes

LItemPtr CreateListOfZonalNodes ( ZonePtr pZ1, ZonePtr pZ2 )	// UNIQUE Nodes of two Zones
																// !!!!! ALLOCATES MEMORY !!!!!
{	FacePtr		pF;		// pointer for loop over all Faces of a Zone
	NodePtr		pN;		// pointer to a Node of a Face that will be saved,
						// if it has not already been saved
	LItemPtr	pNLIFace;	// Node List Item pointer to loop over a Face's Nodes
	int			nFound;		// flag to indicate if a node has already been saved
	LItemPtr	pZonalList;	// Pointer to the Zone's first Node list item, the RETURN VALUE
	int			nN, numFsNs;// to control for loop
	
	pZonalList = (LItemPtr)MakeList (umNODE);	//----- Create the list to hold unique Nodes
	
	if (pZ1 != NULL)	pF = pZ1->p1stFace;		//------------------------------ FIRST Zone
		else			pF = NULL;
		
	while (pF != NULL) //---------- loop over all Faces of the Zone
	{
		pNLIFace = pF->p1stNodeLI;				// get the Node list for this Face
		numFsNs = CountListItems ( pNLIFace );	// count the number of Nodes

		for (nN = 0; nN < numFsNs; nN++) //---------- loop over the list of the Face's Nodes
		{	
			pN = (NodePtr)RetrieveListItem(pNLIFace, umNODE, nN);	// get each Node in order
								  
			nFound = FindEntityInList (pZonalList, umNODE, pN);		// Is the Node already stored?
			
			if ( nFound < 0 ) //---------- If not found, add it to the Zonal Node list
				AddEntityToListEnd (pZonalList, umNODE, pN);
		}
		pF = pF->pZsNextF; //----- advance to the Zone's next Face
	}
	if (pZ2 != NULL)	pF = pZ2->p1stFace;	//------------------------------ SECOND Zone
		else			pF = NULL;
		
	while (pF != NULL) //---------- loop over all Faces of the Zone
	{
		pNLIFace = pF->p1stNodeLI;
		numFsNs = CountListItems ( pNLIFace );	// count the number of Nodes
		
		for (nN = 0; nN < numFsNs; nN++) //---------- loop over the list of the Face's Nodes
		{	
			pN = (NodePtr)RetrieveListItem(pNLIFace, umNODE, nN);	// get each Node in order
								  
			nFound = FindEntityInList (pZonalList, umNODE, pN);	// Is the Node already stored?
			
			if ( nFound < 0 ) //---------- If not found, add it to the Zonal Node list
				AddEntityToListEnd (pZonalList, umNODE, pN);

			pNLIFace = pNLIFace->pNextItem; //----- go to the Face's next Node List's item
		}
		pF = pF->pZsNextF; //----- advance to the Zone's next Face
	}
	return (pZonalList);
} //----- End of CreateListOfZonalNodes

void MakeAFacesSideList (FacePtr pF, SidePtr pFirstSide)
{	//----- Left pointers MUST be correct for this to work properly!
	SidePtr		pS; //----- to loop through a Face's sides
	
	pF->p1stSide = pFirstSide;
	pS			 = pFirstSide;
	while (pS->pLeftSide != pFirstSide)
	{
		pS->pFsNextS = pS->pLeftSide;
		pS			 = pS->pLeftSide;
	}
	pS->pFsNextS = NULL;
}

void MakeAFacesNodeListFromSideList (FacePtr pF)
//----------------------------------------------------------------------
// This function assumes that the Face's Side list is correct.
//----------------------------------------------------------------------
{	LItemPtr	pFsNodeList;
	SidePtr		pS;
	
	pFsNodeList = (LItemPtr)MakeList (umNODE);	// make an empty list
	pS = pF->p1stSide;
	pF->p1stNodeLI = NULL;
	
	while (pS != NULL)
	{	
		AddEntityToListEnd (pFsNodeList, umNODE, pS->pNode1);
		
		pS = pS->pFsNextS;
	}
	
	pF->p1stNodeLI = pFsNodeList;
}

FacePtr FindOppositeFace (FacePtr pFace)
//----------------------------------------------------------------------
// Given a pointer to a Face, return a pointer to the opposite Face of a
// neighboring Zone.  If the original Face is on the boundary, then the
// return value is set to NULL.
//----------------------------------------------------------------------
{	
	SidePtr		pS;
	
	pS = pFace->p1stSide;
	
	if (pS->pOppositeSide == NULL)
		return (NULL);
	else
		return (pS->pOppositeSide->pFace);
}
