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
#include "C2K-Storage.h"
#include "C2K-Lists.h"

#include "C2K-CMG.h"
#include "C2K-KC_Geom.h"
#include "C2K-KC_Check.h"
#include "C2K-KC_Create.h"
#include "C2K-KC_SubDivide.h"

#ifdef USE_MPI
#include "mpi.h"
#endif

void PrintStructSizes ( )
{
	printf("*** Struct sizes:\n");
	printf("Zone     struct = %d bytes\n", sizeof(struct Zone)    );
	printf("Face     struct = %d bytes\n", sizeof(struct Face)    );
	printf("Side     struct = %d bytes\n", sizeof(struct Side)    );
	printf("Edge     struct = %d bytes\n", sizeof(struct Edge)    );
	printf("Node     struct = %d bytes\n", sizeof(struct Node)    );
	printf("Corner   struct = %d bytes\n", sizeof(struct Corner)  );
	printf("ListItem struct = %d bytes\n", sizeof(struct ListItem));
}

void PrintFaceBState (FB_States FBS)
{
	if (FBS == F_UNASSIGNED)
		printf ("UNASSIGNED");
		
	else if (FBS == INTERIOR_FACE)
		printf ("INTERIOR_FACE");
		
	else if (FBS == SEND_FACE)
		printf ("SEND_FACE");

	else if (FBS == RECEIVE_FACE)
		printf ("RECEIVE_FACE");
		
	else if (FBS == PROBLEM_BOUNDARY_FACE)
		printf ("PROBLEM_BOUNDARY_FACE");
}


void DebugBoundaryArrayProcessing ( )
{
	Init_CMG_Problem ( 1 );	// a simple 1 x 2 x 2 single Node mesh with two comm Faces

	InitializeAllStorage ( );
	
	Create_KC_Mesh ( );

//	printf("*** STARTING CACI: After creating KC mesh# 1, and before subdividing.\n");
	CheckAllConnectivityInfo ("After creating KC mesh; before subdividing.\n");
//	printf("*** PASSED CACI\n");

	CalcAllFaceCentroids ( );
	CalcAllZoneCentroids ( );
//	CalculateAllZoneVolumes ( 2 );

	FacePtr		pF;
	pF = pFace(0);
	while (pF != NULL)
	{
		printf ("Face LID = %3d, GID = %d, Face Type = ",pF->nFaceLID, pF->nFaceGID);
		PrintFaceBState (pF->FType);
		printf ("\n");
		pF = pF->pNextFace;
	}
  FreeAllStorage();
  
}

void RunFourTestProblems ( )
{
	int		nP;		// for loop index
	for (nP = 1; nP < 2; nP++)
	{
    printf("PROBLEM %d\n",nP);
    
		Init_CMG_Problem ( nP );

		InitializeAllStorage ( );

		Create_KC_Mesh ( );
	
		printf("*** STARTING CACI: After creating KC mesh# %d, and before subdividing.\n", nP);
		CheckAllConnectivityInfo ("After creating KC mesh; before subdividing.\n");
		printf("*** PASSED CACI\n");

		CalcAllFaceCentroids ( );
		CalcAllZoneCentroids ( );
		CalculateAllZoneVolumes ( 2 );
		
		CheckAllConnectivityInfo ("After subdividing.\n");
		printf("*** PASSED CACI\n\n\n");

    FreeAllStorage ( );
    printf("PROBLEM %d FINISHED\n",nP);
        
	}
}
int main (int argc, char * argv[])
{	
	int		nP;		// for loop index
	
  int myRank = 0;

#ifdef USE_MPI
  MPI_Init(&argc, &argv);

  MPI_Comm_rank(MPI_COMM_WORLD, &myRank);

  CMGDPRINT("Processor id %d reporting in\n",myRank);
#endif
  
	RunFourTestProblems ( );

/* 	DebugBoundaryArrayProcessing ( ); */

#ifdef USE_MPI
  MPI_Finalize( );
#endif

  return 0;
}
