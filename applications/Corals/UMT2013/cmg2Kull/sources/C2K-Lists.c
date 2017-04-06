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

#include "C2K-Storage.h"
#include "C2K-Lists.h"

//============================= GENERIC LIST ROUTINES ============================

LItemPtr MakeList ( umEntity Kind ) //----- Create a list to hold pointers to
									// UM entities. No entities are created or
									// destroyed, only list items.
{	LItemPtr	pLI;
	
	pLI = allocateListItem ( );
	pLI->ListType   = Kind;
	pLI->pNextItem  = pLI->pPriorItem = NULL;
	pLI->pItem      = NULL;
	return (pLI);
}

//================================================================================
void  AddEntityToListEnd (LItemPtr pLI, umEntity Kind, void *pItem)
{	LItemPtr	pNewLI, pLIEnd;
	
	if (pLI == NULL) { printf("***** ERROR Pointer to List is NULL.\n");
		exit (1);}
	if (pLI->ListType != Kind) { printf("***** ERROR Adding entity to wrong list type.\n");
		exit (1);}
	if (pItem == NULL) { printf("***** ERROR Pointer to added object is NULL.\n");
		exit (1);}
		
	if (pLI->pItem == NULL) //----- list is empty
		pLI->pItem = pItem;					// Point the empty list item to the entity
	else
	{	pLIEnd = pLI;						// Start at the first item of the list
		while (pLIEnd->pNextItem != NULL)
			pLIEnd = pLIEnd->pNextItem;		// Move to the end of the list

		pNewLI = allocateListItem ( );	// Make the new list item
		pNewLI->ListType   = Kind;		// Set the list type
		pNewLI->pItem      = pItem;		// Point to the requested entity
									// NOW INSERT INTO LIST AT END
		pNewLI->pPriorItem = pLIEnd;	// Prior points to end of list
		pNewLI->pNextItem  = NULL;		// Next points to NULL
		pLIEnd->pNextItem  = pNewLI;	// Previous end of list points to new item
	}
}

//================================================================================
void  AddEntityToListFront (LItemPtr *ppLI, umEntity Kind, void *pItem)
{	LItemPtr	pLI, pNewLI;
	
	pLI = *ppLI;
	if (pLI == NULL) { printf("***** ERROR Pointer to List is NULL.\n");
		exit (1);}
	if (pLI->ListType != Kind) { printf("***** ERROR Adding entity to wrong list type.\n");
		exit (1);}
	if (pItem == NULL) { printf("***** ERROR Pointer to added object is NULL.\n");
		exit (1);}
		
	if (pLI->pItem == NULL)
		pLI->pItem = pItem;				// Point the empty list item to the entity
	else
	{	pNewLI = allocateListItem ( );	// Make the new list item
		pNewLI->ListType   = Kind;		// Set the list type
		pNewLI->pItem      = pItem;		// New list item points to the requested entity
									// NOW INSERT INTO LIST AT FRONT
		pNewLI->pPriorItem = NULL;		// Prior points to nothing
		pNewLI->pNextItem  = pLI;		// Next points to the OLD start of the list
		pLI->pPriorItem    = pNewLI;	// Previous start of list points to new item
		*ppLI = pNewLI;					// Update the pointer to the list passed in
	}									// by the calling program
}

//================================================================================
int  CountListItems (LItemPtr pLItem)
{	int			Count;
	LItemPtr	pLI;
	
	if (pLItem == NULL) { printf("***** ERROR Pointer to List is NULL.\n");
		exit (1);}
		
	if (pLItem->pItem == NULL) return (0);
	Count = 0;
	pLI = pLItem;
	while (pLI != NULL) //----- loop over all list items
	{	Count = Count + 1;
		pLI = pLI->pNextItem;
	}
	return (Count);
}

//================================================================================
void  *RetrieveListItem (LItemPtr pLItem, umEntity Kind, int numItem) // zero-based index
{	int			Count;
	LItemPtr	pLI;
	
	if (pLItem == NULL) { printf("***** ERROR Pointer to List is NULL.\n");
		exit (1);}
	if (pLItem->ListType != Kind) { printf("***** ERROR Retrieving entity from wrong list type.\n");
		exit (1);}

	if (pLItem->pItem == NULL) return (NULL);
	Count = -1;
	pLI = pLItem;
	while (pLI != NULL)
	{	Count = Count + 1;
		if (Count == numItem) return (pLI->pItem);
		pLI = pLI->pNextItem;
	}
	return (NULL);
}

//================================================================================
int  FindEntityInList (LItemPtr pLItem, umEntity Kind, void *pItem)
{	int			Count;
	LItemPtr	pLI;
	
	if (pLItem == NULL) { printf("***** ERROR Pointer to List is NULL.\n");
		exit (1);}
	if (pLItem->ListType != Kind) { printf("***** ERROR Searching for entity in wrong list type.\n");
		exit (1);}

	if (pLItem->pItem == NULL) return (-1);
	Count = -1;
	pLI = pLItem;
	while (pLI != NULL)
	{	Count = Count + 1;
		if (pLI->pItem == pItem) return (Count);
		pLI = pLI->pNextItem;
	}
	return (-1);
}

//int  RemoveItemFromList (LItemPtr pLItem, umEntity Kind, int numItem) // zero-based index
//{	NOT NEEDED YET
//}

//================================================================================
void  RemoveEntityFromList (LItemPtr *ppLI, umEntity Kind, void *pEntity)
{	
	LItemPtr	pLI;		// to follow pointers in the list
	LItemPtr	pLI2Delete;	// to follow pointers in the list
	int			iRemove, iR;// the index in the list of the Entity to remove

	pLI = *ppLI;			// because the pointer to the first list item may change
	
	if (pLI == NULL)			{ printf("***** ERROR Pointer to List is NULL.\n");
		exit (1);}
	if (pLI->ListType != Kind)	{ printf("***** ERROR Searching for entity in wrong list type.\n");
		exit (1);}
	if (pEntity == NULL)		{ printf("***** ERROR pointer to Entity to remove is NULL.\n");
		exit (1);}
		
	iRemove = FindEntityInList (pLI,  Kind, pEntity);
	if (iRemove < 0)
	{							  printf("***** ERROR Entity to be removed is not found in the list.\n");
		exit(1);
	}
	else if (iRemove == 0)	//----- remove the first list item (which has index 0)
	{
		pLI2Delete = pLI;
		pLI = pLI->pNextItem;	// Point to the next list item
		pLI->pPriorItem = NULL; // set prior pointer for the first item in the list
	}
	else
	{	for (iR = 0; iR < iRemove; iR++) pLI = pLI->pNextItem;	// advance to the item to be removed
		
		pLI->pPriorItem->pNextItem = pLI;						// set prior item's next pointer
		if ( pLI->pNextItem != NULL )
			pLI->pNextItem->pPriorItem = pLI;					// set next item's prior pointer
	}
	
	*ppLI = pLI;
	
	freeListItem ( pLI2Delete ); // free up list item
	
}

//================================================================================
void  DeleteList (LItemPtr pLI) // DOES NOT DELETE ENTITIES, ONLY LIST ITEMS
{	LItemPtr  pNextLI;
	
	while (pLI != NULL)
	{	pNextLI = pLI->pNextItem;
		freeListItem ( pLI );
		pLI = pNextLI;
	}
}

//----- variables to control SEQUENTIAL list access
LItemPtr	pList_Head, pListItem;

//================================================================================
void SetupSeqListAccess	 ( LItemPtr pLI, umEntity Kind )
{
	if (pLI == NULL) {; printf("ERROR NULL list pointer in SetupSeqListAccess.\n");
		exit (1);}
	if (pLI->ListType != Kind) {; printf("ERROR List type not correct in SetupSeqListAccess.\n");
		exit (1);}
	pList_Head = pLI;
	pListItem  = pLI;
}

//================================================================================
void *GetNextEntityFromList ( LItemPtr pLH, umEntity Kind )
{	LItemPtr	pLI;
	
	if (pLH == NULL) {; printf("ERROR NULL list pointer in GetNextEntityFromList.\n");
		exit (1);}
	if (pLH->ListType != Kind) {; printf("ERROR List type not correct in GetNextEntityFromList.\n");
		exit (1);}
	if (pLH != pList_Head) {; printf("ERROR List pointer != list head in GetNextEntityFromList.\n");
		exit (1);}
	
	if (pListItem		 == NULL) return (NULL);
	if (pListItem->pItem == NULL) return (NULL);
	
	pLI = pListItem;
	pListItem = pListItem->pNextItem;
	return (pLI->pItem);
}
