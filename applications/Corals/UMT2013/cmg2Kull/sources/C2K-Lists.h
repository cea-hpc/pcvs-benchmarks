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
#ifndef C2KLIST
#define C2KLIST

#include "C2K-Storage.h"

//=============================================== GENERIC LIST FUNCTION PROTOTYPES
LItemPtr	MakeList			 (				  umEntity Kind			    );
void		AddEntityToListFront (LItemPtr *ppLI, umEntity Kind, void *pItem);
void		AddEntityToListEnd   (LItemPtr   pLI, umEntity Kind, void *pItem);
int			CountListItems		 (LItemPtr   pLI							);
void	   *RetrieveListItem	 (LItemPtr   pLI, umEntity Kind, int numItem);
int		    FindEntityInList	 (LItemPtr   pLI, umEntity Kind, void *pItem);	// replaces IndexOfNodeInList
//int		RemoveItemFromList   (LItemPtr   pLI, umEntity Kind, int numItem);
void	    RemoveEntityFromList (LItemPtr *ppLI, umEntity Kind, void *pItem);	// NEW
void	    SetupSeqListAccess	 (LItemPtr   pLI, umEntity Kind				);
void	   *GetNextEntityFromList(LItemPtr   pLI, umEntity Kind				);
void	    DeleteList			 (LItemPtr   pLI							);	// NEW

#endif
