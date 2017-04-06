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
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "C2K-CMG.h"


#ifdef CMG_FAKE
//======================================================================
//                                                           CMG STORAGE
#define MAXN          200
#define MAXZ         MAXN
#define MAXF       3*MAXN
#define MAXP_DOM        5
#define MAXFperP_DOM    5

int		valid = 0;

//-------------------------------------------------- arrays to define NODES
int		nN;				// the number of nodes
int		N_GID[MAXN];	// A list of GLOBAL Node IDs for ALL nodes, both owned and not
int		N_Offset;		// The 0-origin offset of where UNOWNED nodes begins

double	xp[MAXN], yp[MAXN], zp[MAXN];	// the x, y, and z node positions

//-------------------------------------------------- arrays to define ZONES
int		nZ;				// the number of zones
int		Z_GID[MAXZ];	// A list of GLOBAL zone IDs for all zones

int		ZN[MAXZ][8];	// a zone's ORDERED list of nodes

int		ZT[MAXZ];		// a zone's type

//-------------------------------------------------- arrays to define BOUNDARY FACES
int		nBF;				// # of boundary faces, both owned and not
int		BF_GID[MAXF];		// A list of GLOBAL boundary face IDs, both owned and not
int		BF_N_GID[MAXF][4];	// The GLOBAL node IDs of a given boundary face global ID

//-------------------------------------------------- arrays to define PARALLEL DOMAIN DATA

int		Num_Neighbor_Domains;				// # of neighboring COMM domains
int		COMM_Domain_IDs		[MAXP_DOM];		// domain IDs of COMM neighbors
int		Num_Boundary_Faces	[MAXP_DOM];		// # of shared ZONE Faces for each COMM neighbor
int		COMM_F_GID			[MAXP_DOM][MAXFperP_DOM];	// GIDs of Faces shared with a domain

//-------------------------------------------------- arrays to test correctness

int		CorrectResults [8];	// the number of various objects created

int		ZNP[MAXZ];			// the number of permutations possible for each zone
int		NumP_HEX = 24;
int		NumP_TET = 12;
int		NumP_PRI = 6;
int		NumP_PYR = 4;
int		ZCP[MAXZ];			// the current permutation for each zone



void Init_Problem_1 ( )
//======================================================================
// XZones = 2, YZones = 1, ZZones = 2, ALL hexahedra
{	int	iZ, iN, iX;	// loop indicies

	int	ZN_0[4][8] = {{ 0, 1, 4, 3, 6, 7,10, 9},	// NOTE!! MUST be 0-origin indices
					  { 1, 2, 5, 4, 7, 8,11,10},
					  { 6, 7,10, 9,12,13,16,15},
					  {10, 7, 8,11,16,13,14,17}};
	
	double p_0[18][3] = {{0.,0.,0.},{1.,0.,0.},{2.,0.,0.},
						 {0.,1.,0.},{1.,1.,0.},{2.,1.,0.},
						 {0.,0.,1.},{1.,0.,1.},{2.,0.,1.},
						 {0.,1.,1.},{1.,1.,1.},{2.,1.,1.},
						 {0.,0.,2.},{1.,0.,2.},{2.,0.,2.},
						 {0.,1.,2.},{1.,1.,2.},{2.,1.,2.} };
	nZ  =  4;
	nN  = 18;
	N_Offset = 12;
	
	int CorrectResults_1 [8] = {   18,    4,   33,   24,   96,      4,    16,     32};
					// Owned:   Nodes Zones Edges Faces Sides OFPairs BFaces Corners
	
	for (iZ = 0; iZ < nZ; iZ++)
		for (iN = 0; iN < 8; iN++)
			ZN[iZ][iN] = ZN_0[iZ][iN];	// transfer ORDERED nodes
	
	for (iZ = 0; iZ < nZ; iZ++)
	{	ZT[iZ] = CMG_HEX;				// set zone type to hex
		Z_GID[iZ] = iZ;					// set global ID
	}
	
	for (iN = 0; iN < nN; iN++)
	{
		xp[iN] = p_0[iN][0];			// transfer positions 
		yp[iN] = p_0[iN][1];
		zp[iN] = p_0[iN][2];
		N_GID[iN] = iN;					// Nodal global ID
	}
	
	nBF =  2;
	BF_GID[0] = 40;	// global IDs of the two boundary faces
	BF_GID[1] = 41;	// for now, these can be anything
	BF_N_GID[0][0] = 12;	BF_N_GID[0][1] = 13;	BF_N_GID[0][2] = 15;	BF_N_GID[0][3] = 16;
	BF_N_GID[1][0] = 17;	BF_N_GID[1][1] = 16;	BF_N_GID[1][2] = 13;	BF_N_GID[1][3] = 14;
	
	Num_Neighbor_Domains  = 1;	//-------------------- Here starts the boundary neighbor COMM array data

	COMM_Domain_IDs   [0] = 7;
	Num_Boundary_Faces[0] = 2;
	
	COMM_F_GID [0][0] = 40;	// GIDs of Faces shared with a domain
	COMM_F_GID [0][1] = 41;	// GIDs of Faces shared with a domain

	for (iX = 0; iX < 8; iX++)
		CorrectResults[iX] = CorrectResults_1[iX];	// transfer the correct answer
	
	for (iZ = 0; iZ < nZ; iZ++)
	{	ZNP[iZ] = NumP_HEX;				// Permutation # per zone
		ZCP[iZ] = 0;					// initialize permutation state
	}
	
	valid = 1;
/* 	printf("CMG initialization completed for ==========> PROBLEM # 1\n"); */
}

void Init_Problem_2 ( )
//======================================================================
// 1 Hexahedron, 1 Tetrahedron, 1 Prism, 1 Pyramid
{	int	iZ, iN, iX;

	int	ZN_0[4][8] = {{ 0, 1, 3, 4, 5, 6, 8, 9},	// NOTE!! MUST be 0-origin indices
					  { 1, 2, 3, 6, 7, 8, 0, 0},
					  { 5, 6, 8, 9,10, 0, 0, 0},
					  { 6, 7, 8,10, 0, 0, 0, 0}};
	
	double p_0[11][3] = {{0.00,0.00,0.00},{1.00,0.00,0.00},{1.50,0.50,0.00},
						 {1.00,1.00,0.00},{0.00,1.00,0.00},{0.00,0.00,1.00},
						 {1.00,0.00,1.00},{1.50,0.50,1.25},{1.00,1.00,1.00},
						 {0.00,1.00,1.00},{0.50,0.50,1.75}                  };

	int CorrectResults_2 [8] = {   11,    4,   22,   20,   70,      4,    12,     23};
					// Owned:   Nodes Zones Edges Faces Sides OFPairs BFaces Corners
	nZ = 4;
	nN = 11;
	N_Offset = 7;
	
	for (iZ = 0; iZ < nZ; iZ++)
		for (iN = 0; iN < 8; iN++)
			ZN[iZ][iN] = ZN_0[iZ][iN];	// transfer ORDERED nodes
	
	ZT[0] = CMG_HEX;	// Hexagon
	ZT[1] = CMG_PRI;	// Prism
	ZT[2] = CMG_PYR;	// Pyramid
	ZT[3] = CMG_TET;	// Tetrahedron
	
	ZNP[0] = NumP_HEX;	// Hexagon
	ZNP[1] = NumP_PRI;	// Prism
	ZNP[2] = NumP_PYR;	// Pyramid
	ZNP[3] = NumP_TET;	// Tetrahedron
	
	for (iZ = 0; iZ < nZ; iZ++)
		Z_GID[iZ] = iZ;					// set global ID
	
	for (iN = 0; iN < nN; iN++)
	{
		xp[iN] = p_0[iN][0];			// transfer positions 
		yp[iN] = p_0[iN][1];
		zp[iN] = p_0[iN][2];
		N_GID[iN] = iN;					// Nodal global ID
	}
	
	nBF =  2;
	BF_GID[0] = 40;	// global IDs of the two boundary faces
	BF_GID[1] = 41;	// for now, these can be anything
	BF_N_GID[0][0] =  7;	BF_N_GID[0][1] =  8;	BF_N_GID[0][2] = 10;	BF_N_GID[0][3] =  0;
	BF_N_GID[1][0] =  8;	BF_N_GID[1][1] =  9;	BF_N_GID[1][2] = 10;	BF_N_GID[1][3] =  0;
	
	Num_Neighbor_Domains  = 2;	//-------------------- Here starts the boundary neighbor COMM array data

	COMM_Domain_IDs   [0] = 6;
	Num_Boundary_Faces[0] = 1;
	
	COMM_Domain_IDs   [1] = 7;
	Num_Boundary_Faces[1] = 1;
	
	COMM_F_GID [0][0] = 40;	// GIDs of Faces shared with a domain
	COMM_F_GID [1][0] = 41;	// GIDs of Faces shared with a domain

	for (iX = 0; iX < 8; iX++)
		CorrectResults[iX] = CorrectResults_2[iX];	// transfer the correct answer
	
	for (iZ = 0; iZ < nZ; iZ++)
		ZCP[iZ] = 0;					// initialize permutation state

	valid = 1;
/* 	printf("CMG initialization completed for ==========> PROBLEM # 2\n"); */
}

void Init_Problem_3 ( )
//======================================================================
// 7 Zones, all hexahedra; like one box inside another
{	int	iZ, iN, iX;	// loop indicies

	int	ZN_0[7][8] = {{ 0, 1, 9, 8, 4, 5,13,12},	// NOTE!! MUST be 0-origin indices
					  { 1, 2,10, 9, 5, 6,14,13},
					  { 2, 3,11,10, 6, 7,15,14},
					  { 3, 0, 8,11, 7, 4,12,15},
					  { 0, 1, 2, 3, 8, 9,10,11},
					  {12,13,14,15, 4, 5, 6, 7},
					  { 8, 9,10,11,12,13,14,15}};
	
	double p_0[16][3] = {{0.00,0.00,0.00},{1.00,0.00,0.00},{1.00,1.00,0.00},
						 {0.00,1.00,0.00},{0.00,0.00,1.00},{1.00,0.00,1.00},
						 {1.00,1.00,1.00},{0.00,1.00,1.00},{0.25,0.25,0.25},
						 {0.75,0.25,0.25},{0.75,0.75,0.25},{0.25,0.75,0.25},
						 {0.25,0.25,0.75},{0.75,0.25,0.75},{0.75,0.75,0.75},
						 {0.25,0.75,0.75} };
	nZ  =  7;
	nN  = 16;
	N_Offset = 12;
	
	int CorrectResults_3 [8] = {   16,    7,   32,   42,  168,     18,     6,     56};
					// Owned:   Nodes Zones Edges Faces Sides OFPairs BFaces Corners
	
	for (iZ = 0; iZ < nZ; iZ++)
		for (iN = 0; iN < 8; iN++)
			ZN[iZ][iN] = ZN_0[iZ][iN];	// transfer ORDERED nodes
	
	for (iZ = 0; iZ < nZ; iZ++)
	{	ZT[iZ] = CMG_HEX;				// set zone type to hex
		Z_GID[iZ] = iZ;					// set global ID
	}
	
	for (iN = 0; iN < nN; iN++)
	{
		xp[iN] = p_0[iN][0];			// transfer positions 
		yp[iN] = p_0[iN][1];
		zp[iN] = p_0[iN][2];
		N_GID[iN] = iN;					// Nodal global ID
	}
	
	nBF =  2;
	BF_GID[0] = 40;	// global IDs of the two boundary faces
	BF_GID[1] = 41;	// for now, these can be anything
	BF_N_GID[0][0] =  0;	BF_N_GID[0][1] =  1;	BF_N_GID[0][2] =  5;	BF_N_GID[0][3] =  4;
	BF_N_GID[1][0] =  1;	BF_N_GID[1][1] =  2;	BF_N_GID[1][2] =  6;	BF_N_GID[1][3] =  5;
	
	Num_Neighbor_Domains  = 1;	//-------------------- Here starts the boundary neighbor COMM array data

	COMM_Domain_IDs   [0] = 7;
	Num_Boundary_Faces[0] = 2;
	
	COMM_F_GID [0][0] = 40;	// GIDs of Faces shared with a domain
	COMM_F_GID [0][1] = 41;	// GIDs of Faces shared with a domain

	for (iX = 0; iX < 8; iX++)
		CorrectResults[iX] = CorrectResults_3[iX];	// transfer the correct answer
	
	for (iZ = 0; iZ < nZ; iZ++)
	{	ZNP[iZ] = NumP_HEX;				// Permutation # per zone
		ZCP[iZ] = 0;					// initialize permutation state
	}
	
	valid = 1;
	printf("CMG initialization completed for ==========> PROBLEM # 3\n");
}
void Init_Problem_4 ( )
// 13 Zones, all hexahedra; like one box inside another, inside yet another.
{	int	iZ, iN, iX;	// loop indicies

	int	ZN_0[13][8] = {{ 0, 1, 9, 8, 4, 5,13,12},	// NOTE!! MUST be 0-origin indices
					   { 1, 2,10, 9, 5, 6,14,13},
					   { 2, 3,11,10, 6, 7,15,14},
					   { 3, 0, 8,11, 7, 4,12,15},
					   { 0, 1, 2, 3, 8, 9,10,11},
					   {12,13,14,15, 4, 5, 6, 7},
					   { 8, 9,10,11,12,13,14,15},	//  6; the innermost hex
					   { 0, 3,19,16, 4, 7,23,20},	//  7
					   { 1,17,18, 2, 5,21,22, 6},	//  8
					   {16,17,18,19, 0, 1, 2, 3},	//  9
					   { 4, 5, 6, 7,20,21,22,23},	// 10
					   { 1, 0,16,17, 5, 4,20,21},	// 11
					   { 3, 2,18,19, 7, 6,22,23} };	// 12
					   
					   
					   
	
	double p_0[24][3] = {{0.00,0.00,0.00},{1.00,0.00,0.00},{1.00,1.00,0.00},{0.00,1.00,0.00},	// 0-3; "middle block"
						 {0.00,0.00,1.00},{1.00,0.00,1.00},{1.00,1.00,1.00},{0.00,1.00,1.00},	// 4-7; "middle block"

						 {0.25,0.25,0.25},{0.75,0.25,0.25},{0.75,0.75,0.25},{0.25,0.75,0.25},	//  8-11; "innermost block"
						 {0.25,0.25,0.75},{0.75,0.25,0.75},{0.75,0.75,0.75},{0.25,0.75,0.75},	// 12-15; "innermost block"
						 
						 {-1.0,-1.0,-1.0},{ 2.0,-1.0,-1.0},{ 2.0, 2.0,-1.0},{-1.0, 2.0,-1.0},	// 16-19; "outer block"
						 {-1.0,-1.0, 2.0},{ 2.0,-1.0, 2.0},{ 2.0, 2.0, 2.0},{-1.0, 2.0, 2.0} };	// 20-23; "outer block"
	nZ  = 13;
	nN  = 24;
	N_Offset = 20;
	
	int CorrectResults_4 [8] = {   24,   13,   52,   78,  312,     36,     6,    104};
					// Owned:   Nodes Zones Edges Faces Sides OFPairs BFaces Corners
	
	for (iZ = 0; iZ < nZ; iZ++)
		for (iN = 0; iN < 8; iN++)
			ZN[iZ][iN] = ZN_0[iZ][iN];	// transfer ORDERED nodes
	
	for (iZ = 0; iZ < nZ; iZ++)
	{	ZT[iZ] = CMG_HEX;				// set zone type to hex
		Z_GID[iZ] = iZ;					// set global ID
	}
	
	for (iN = 0; iN < nN; iN++)
	{
		xp[iN] = p_0[iN][0];			// transfer positions 
		yp[iN] = p_0[iN][1];
		zp[iN] = p_0[iN][2];
		N_GID[iN] = iN;					// Nodal global ID
	}
	
	nBF =  2;
	BF_GID[0] = 40;	// global IDs of the two boundary faces
	BF_GID[1] = 41;	// for now, these can be anything
	BF_N_GID[0][0] = 16;	BF_N_GID[0][1] = 17;	BF_N_GID[0][2] = 18;	BF_N_GID[0][3] = 19;
	BF_N_GID[1][0] = 16;	BF_N_GID[1][1] = 17;	BF_N_GID[1][2] = 21;	BF_N_GID[1][3] = 20;
	
	Num_Neighbor_Domains  = 2;	//-------------------- Here starts the boundary neighbor COMM array data

	COMM_Domain_IDs   [0] = 6;
	Num_Boundary_Faces[0] = 1;
	
	COMM_Domain_IDs   [1] = 7;
	Num_Boundary_Faces[1] = 1;
	
	COMM_F_GID [0][0] = 40;	// GIDs of Faces shared with a domain
	COMM_F_GID [1][0] = 41;	// GIDs of Faces shared with a domain

	for (iX = 0; iX < 8; iX++)
		CorrectResults[iX] = CorrectResults_4[iX];	// transfer the correct answer
	
	for (iZ = 0; iZ < nZ; iZ++)
	{	ZNP[iZ] = NumP_HEX;				// Permutation # per zone
		ZCP[iZ] = 0;					// initialize permutation state
	}
	
	valid = 1;
	printf("CMG initialization completed for ==========> PROBLEM # 4\n");
}


//======================================================================
void cnmnoda (int *nnodes)	// Get the number of ALL nodes on this domain,
							// both OWNED and UNOWNED
{	
	if (valid != 1) { printf("ERROR CMG NOT initialized.\n"); exit(1); }
	*nnodes = nN;
}

//======================================================================
void cnodsa (int *nnodes, int *offset)	// Get the GID of ALL nodes on this domain,
										// both OWNED and UNOWNED, AND the offset
										// to where in the list UNOWNED nodes start.
{	int	iN; // loop index
	
	if (valid != 1) { printf("ERROR CMG NOT initialized.\n"); exit(1); }
	
	for (iN = 0; iN < nN; iN++)
		nnodes[iN] = N_GID[iN];
	
	*offset = N_Offset;
}

//======================================================================
void cnodpos (int nodeid, double *x, double *y, double *z)	//Get the node position for the given node
{
	if (valid != 1) { printf("ERROR CMG NOT initialized.\n"); exit(1); }
	*x = xp[nodeid];
	*y = yp[nodeid];
	*z = zp[nodeid];
}

//======================================================================
void cnumzns (int *nzones)	// Get the number of zones on this domain
{
	if (valid != 1) { printf("ERROR CMG NOT initialized.\n"); exit(1); }
	*nzones = nZ;
}

//======================================================================
void czns (int *nzones)	// Get the list of GIDs for zones on this domain
{
	int		iZ;	// loop index
	
	if (valid != 1) { printf("ERROR CMG NOT initialized.\n"); exit(1); }
	
	for (iZ = 0; iZ < nZ; iZ++)
		nzones[iZ] = Z_GID[iZ];
}

//======================================================================
void cgetznn (int gloid, int *nodes)	// Get the nodes in order for the given zone
{	int	iN;
	
	if (valid != 1) { printf("ERROR CMG NOT initialized.\n"); exit(1); }
	for (iN = 0; iN < 8; iN++)
		nodes[iN] = ZN[gloid][iN];
}

//======================================================================
void cgetztp (int gloid, cZoneType *typ)	// Get the zone type for a given zone
{
	if (valid != 1) { printf("ERROR CMG NOT initialized.\n"); exit(1); }
	*typ = ZT[gloid];
}

//======================================================================
void cnumbdf (int *nfaces)	// Get # of boundary faces on this domain; owned + unowned
{
	if (valid != 1) { printf("ERROR CMG NOT initialized.\n"); exit(1); }
	
	*nfaces = nBF;
}

//======================================================================
void cbdrfc (int *faces)	// Get the list of boundary face GIDs; owned + unowned
{
	int		iBF;	// loop index
	
	if (valid != 1) { printf("ERROR CMG NOT initialized.\n"); exit(1); }
	
	for (iBF = 0; iBF < nBF; iBF++)
		faces[iBF] = BF_GID[iBF];
}

//======================================================================
void cfcnda (int facid, int *nodid)	// get four node GIDs for any FACE,
									// not just boundary faces
{					// !! for this FAKE CMG, facid is only an index
					//    into the domain's boundary faces
	int		iBFN;	// loop index
	
	if (valid != 1) { printf("ERROR CMG NOT initialized.\n"); exit(1); }
	
	for (iBFN = 0; iBFN < 4; iBFN++)
		nodid [iBFN] = BF_N_GID[facid][iBFN];	// for a given face GID, return the node GIDs
}

//-------------------------------------------------- COMM neighbor query functions

//======================================================================
void crecfsz ( int *size )		// the size of the domain ID and # of Faces per domain arrays
{
	*size = Num_Neighbor_Domains;
}

//======================================================================
void crecfar (int *domar, int *fcszar )	// domain ID array and # of Faces per domain
{
	int		num_D;	// to loop over sharing domains
	
	for (num_D = 0; num_D < Num_Neighbor_Domains; num_D++)	// loop over sharing domains
	{
		domar [num_D] = COMM_Domain_IDs   [num_D];			// return domain ID
		fcszar[num_D] = Num_Boundary_Faces[num_D];			// # of shared Faces
	}
}

//======================================================================
void crecf ( int domid, int *faces )	// get the GIDs of the COMM Faces for domain # domid
{
	int		num_D, ixF;		// for looping over domains and Faces
	
	for (num_D = 0; num_D < Num_Neighbor_Domains; num_D++)	//  loop over domain storage
	{
		if ( domid == COMM_Domain_IDs[num_D])						// when the domain ID is found, ...
		{
			for ( ixF = 0; ixF < Num_Boundary_Faces[num_D]; ixF++)	// loop over shared Faces
			{
				faces[ixF] = COMM_F_GID[num_D][ixF];				// store Face's GIDs into faces array
			}
		}
	}
}



//================================== END OF CMG_FAKE ====================================


#else

void readInput( FILE* );

void Init_CMG_Problem_From_File  (const char* meshFile)
//======================================================================
{
  FILE *inputDeck;
  char fileName[100];

  strcpy(fileName,meshFile);

/*   printf("using input deck:  %s\n",fileName); */
  
  /*Open the file to parse */
  inputDeck = fopen(fileName, "r");

  /*print the prompt*/
  CMGDPRINT("CMG > ");
  
  readInput(inputDeck);
 
  int success = fclose(inputDeck);

  bcastinp( );
   
  cgenmesh();

/*   printf("CMG initialization completed for ==========> PROBLEM # 1\n"); */

}

void Init_Problem_1 ( )
//======================================================================
// XZones = 2, YZones = 1, ZZones = 2, ALL hexahedra
{
  FILE *inputDeck;
  char fileName[100];

  strcpy(fileName,"problem1.cmg");

/*   printf("using input deck:  %s\n",fileName); */
  
  /*Open the file to parse */
  inputDeck = fopen(fileName, "r");

  /*print the prompt*/
  CMGDPRINT("CMG > ");
  
  readInput(inputDeck);
 
  int success = fclose(inputDeck);

  bcastinp( );
   
  cgenmesh();

/*   printf("CMG initialization completed for ==========> PROBLEM # 1\n"); */
}

void Init_Problem_2 ( )
//======================================================================
// 1 Hexahedron, 1 Tetrahedron, 1 Prism, 1 Pyramid
{	
}

void Init_Problem_3 ( )
//======================================================================
// 7 Zones, all hexahedra; like one box inside another
{	
}

void Init_Problem_4 ( )
//======================================================================
// 7 Zones, all hexahedra; like one box inside another
{	
}

void readInput(FILE *input)
{  
  
  if (input != NULL) {
    /* Assign the file to the variable that is read by the parser */
    csetinp( input );
  }
  else {
    /*Leave it set to stdin */
  }

  /* Call the CMG parsing routine */
  creadinp();
  /* Call the CMG input printing routine */
  cprntinp();
/*   printf("FINISHED READING INPUT FILE.\n"); */
  
}


#endif


void Init_CMG_Problem (int ProblemNumber)
//======================================================================
{	
	
	if (ProblemNumber == 1)	
		Init_Problem_1 ( );
	else if (ProblemNumber == 2)	
		Init_Problem_2 ( );
	else if (ProblemNumber == 3)	
		Init_Problem_3 ( );
	else if (ProblemNumber == 4)	
		Init_Problem_4 ( );
	else
	{	printf("ERROR Invalid problem number requested.\n");
		exit(1);
	}
}

