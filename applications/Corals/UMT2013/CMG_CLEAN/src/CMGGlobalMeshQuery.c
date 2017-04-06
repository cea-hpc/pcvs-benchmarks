/*
// Copyright 2006 The Regents of the University of California.
// All rights reserved.
//--------------------------------------------------------------------------
//--------------------------------------------------------------------------
//
// This work was produced at the University of California, Lawrence
// Livermore National Laboratory (UC LLNL) under contract no.
// W-7405-ENG-48 (Contract 48) between the U.S. Department of Energy
// (DOE) and the Regents of the University of California (University)
// for the operation of UC LLNL.  The rights of the Federal Government are
// reserved under Contract 48 subject to the restrictions agreed upon by
// the DOE and University as allowed under DOE Acquisition Letter 97-1.
//
//--------------------------------------------------------------------------
// Revision: Mon Nov 13 17:16:40 PST 2006
// Author: Benjamin T. Grover
// Comment: Initial implementation of CMGGlobalMeshQuery.c
//--------------------------------------------------------------------------
*/

/*!\file CMGGlobalMeshQuery.c
  \brief Global Query functions on the mesh

*/
#include <stdlib.h>
#include "CMGMeshQuery.h"
#include "meshAndInputData.h"
#include "dataTypes.h"
#include "cmg.h"
#include "CMGDebug.h"
#include "CMGMeshTopology.h"


#ifdef USE_MPI
#include "mpi.h"
#endif


/* sms query
 */
/*! Get the sms index for a given task
 */
void cgetsms(int *i, int *j, int *k)
{
    int domainId =-1,i1=-1,j1=-1,k1=-1;

  #ifdef USE_MPI
  extern int blockProcessorMap[CMG_MAX_BLOCK_DEFINITIONS];
  extern MeshCellContainer cellContainer;
  
  MPI_Comm_rank(MPI_COMM_WORLD, &domainId);
  i1 = cellContainer.cellList[blockProcessorMap[domainId]].i;
  j1 = cellContainer.cellList[blockProcessorMap[domainId]].j;
  k1 = cellContainer.cellList[blockProcessorMap[domainId]].k;

  (*i)=i1;
  (*j)=j1;
  (*k)=k1;
  
  #else
  CMGDPRINT("cgetsms can only be called with more than one domain");
  
  #endif
}

/*!
  Fortran cgetsms
*/
void cgetsms_(int *i, int *j, int *k)
{
    cgetsms(i, j, k);
}



/*Boundary Face query functions
 */
/*!Get the number of boundary faces on this domain, whether owned or not
 */
void cnumbdf(int *nfaces)
{

  int domainId =-1;
  int i,j,k;
  int ni,nj,nk;
  
  
  
  #ifdef USE_MPI
  extern int blockProcessorMap[CMG_MAX_BLOCK_DEFINITIONS];
  extern MeshCellContainer cellContainer;
  extern NumZones numZones;

  MPI_Comm_rank(MPI_COMM_WORLD, &domainId);
  i = cellContainer.cellList[blockProcessorMap[domainId]].i;
  j = cellContainer.cellList[blockProcessorMap[domainId]].j;
  k = cellContainer.cellList[blockProcessorMap[domainId]].k;

  *nfaces = 0;

  ni = numZones.iZones[i];
  nj = numZones.jZones[j];
  nk = numZones.kZones[k];
  

  (*nfaces) = ni * nj *2;
  (*nfaces) = (*nfaces) + (nj*nk*2);
  (*nfaces) = (*nfaces) + (nk*ni*2);
  #else
  CMGDPRINT("cnumbdf can only be called with more than one domain");
  
  #endif
}


/*!
  Fortran cnumbdf( ) */
void cnumbdf_(int *nfaces)
{
  cnumbdf(nfaces);
}


/*!Get the list of boundary face ids on this domain, whether owned or not
 */
void cbdrfc(int *faces)
{
    
  int i,j;
  int counter = 0;
  int domainId = -1;
  
  
  

    /*Get the cell for this domain, and then ask for the mesh faces
      on that cell, then move forward by asking for the faces on each
      face*/

#ifdef USE_MPI
  extern int blockProcessorMap[CMG_MAX_BLOCK_DEFINITIONS];
  extern MeshCellContainer cellContainer;
  extern MeshFaceContainer faceContainer;
  
  MPI_Comm_rank(MPI_COMM_WORLD, &domainId);
  for(i=0;i<6;++i){
    int faceId = cellContainer.cellList[blockProcessorMap[domainId]].meshFaceIds[i];
    int totalFaces = faceContainer.faceList[faceId].numFaces;
    int baseId = faceContainer.faceList[faceId].baseFaceId;
    for(j=0;j<totalFaces;++j){
      faces[counter++] = baseId+j;
    }
    
    
  } 
#endif
}


/* Fortran cbdrfc( )*/
void cbdrfc_(int *faces)
{
  cbdrfc(faces);
}


/*!Get the four nodes for the given boundary face that is on this domain
 */
void cfcnda(int facid, int *nodid)
{
  int meshFaceId = -1;
  int i,j,offset,ni,nj;
  int domainId = -1;
  
  
  
    /*Lets figure out what face of the 6 faces that touch this domain this
      face is on*/
  #ifdef USE_MPI
  extern int blockProcessorMap[CMG_MAX_BLOCK_DEFINITIONS];
  extern MeshCellContainer cellContainer;
  extern MeshFaceContainer faceContainer;
  extern NumZones numZones;
  
  MPI_Comm_rank(MPI_COMM_WORLD, &domainId);

  for(i=0;i<6;++i){
    int currentMeshFaceId = cellContainer.cellList[blockProcessorMap[domainId]].meshFaceIds[i];
    int baseId = faceContainer.faceList[currentMeshFaceId].baseFaceId;
    int numFaces = faceContainer.faceList[currentMeshFaceId].numFaces;
    if(facid>=baseId && facid<(baseId+numFaces)){
        meshFaceId = currentMeshFaceId;
        offset = i;
        break;
    }
  }

  MeshCell thisCell = cellContainer.cellList[blockProcessorMap[domainId]];
  int cellId = getCellId(thisCell.i,thisCell.j,thisCell.k);
  int localId = facid - faceContainer.faceList[meshFaceId].baseFaceId;
    
/*The faces on the cell are ordered
  i,i+1,j,j+1,k,k+1*/
  if(offset == 0){
      /*On an i face the j will remain j, k will become i for us to figure out
        our information.*/
    
      /*So we need number of jzone and number of kzones(will be i)
       */
    nj = numZones.jZones[thisCell.j];
    ni = numZones.kZones[thisCell.k];

    i = localId / nj;
    j = localId - (i*nj);

      /*This is the local IJK for the node, on this cell*/
    nodid[0] = getNodeFromIJK(0,j,i,cellId);
    nodid[1] = getNodeFromIJK(0,j,i+1,cellId);
    nodid[2] = getNodeFromIJK(0,j+1,i+1,cellId);
    nodid[3] = getNodeFromIJK(0,j+1,i,cellId);

      /*returning face nodes in right hand rule order, see CMGMeshQuery::getznn
        to see the set up of these
      */
    
  }
  else if(offset == 1){

    /*On an i face the j will remain j, k will become i for us to figure out
        our information.*/
    
      /*So we need number of jzone and number of kzones(will be i)
       */
    nj = numZones.jZones[thisCell.j];
    ni = numZones.kZones[thisCell.k];

    i = localId / nj;
    j = localId - (i*nj);

      /*our i is at imax for this cell*/

    nodid[0] = getNodeFromIJK(numZones.iZones[thisCell.i],j,i,cellId);
    nodid[1] = getNodeFromIJK(numZones.iZones[thisCell.i],j+1,i,cellId);
    nodid[2] = getNodeFromIJK(numZones.iZones[thisCell.i],j+1,i+1,cellId);
    nodid[3] = getNodeFromIJK(numZones.iZones[thisCell.i],j,i+1,cellId);    
  }
  else if(offset == 2){
    ni = numZones.iZones[thisCell.i];
    nj = numZones.kZones[thisCell.k];

    i = localId/nj;
    j = localId - (i*nj);

    nodid[0] = getNodeFromIJK(i,0,j,cellId);
    nodid[1] = getNodeFromIJK(i+1,0,j,cellId);
    nodid[2] = getNodeFromIJK(i+1,0,j+1,cellId);
    nodid[3] = getNodeFromIJK(i,0,j+1,cellId);
    
    
    
  }
  else if(offset == 3){
    ni = numZones.iZones[thisCell.i];
    nj = numZones.kZones[thisCell.k];

    i = localId/nj;
    j = localId - (i*nj);

    nodid[0] = getNodeFromIJK(i,numZones.jZones[thisCell.j],j,cellId);
    nodid[1] = getNodeFromIJK(i,numZones.jZones[thisCell.j],j+1,cellId);
    nodid[2] = getNodeFromIJK(i+1,numZones.jZones[thisCell.j],j+1,cellId);
    nodid[3] = getNodeFromIJK(i+1,numZones.jZones[thisCell.j],j,cellId); 
    
  }
  else if(offset == 4){
    ni = numZones.iZones[thisCell.i];
    nj = numZones.jZones[thisCell.j];

    i = localId/nj;
    j = localId - (i*nj);

    nodid[0] = getNodeFromIJK(i,j+1,0,cellId);
    nodid[1] = getNodeFromIJK(i+1,j+1,0,cellId);
    nodid[2] = getNodeFromIJK(i+1,j,0,cellId);
    nodid[3] = getNodeFromIJK(i,j,0,cellId); 
  }
  else{
    ni = numZones.iZones[thisCell.i];
    nj = numZones.jZones[thisCell.j];

    i = localId/nj;
    j = localId - (i*nj);

    nodid[0] = getNodeFromIJK(i,j+1,numZones.kZones[thisCell.k],cellId);
    nodid[1] = getNodeFromIJK(i,j,numZones.kZones[thisCell.k],cellId);
    nodid[2] = getNodeFromIJK(i+1,j,numZones.kZones[thisCell.k],cellId);
    nodid[3] = getNodeFromIJK(i+1,j+1,numZones.kZones[thisCell.k],cellId);
    
  }
  
  #endif
  
}


/*!Fortran cfcnda( )*/
void cfcnda_(int *facid, int *nodid)
{
  cfcnda((*facid),nodid);
}




/* Node query functions
 */
/*!
  This returns a count of ALL the nodes that are in or touch the domain
  boundary
*/
void cnmnoda (int *nnodes)
{

  int domainId = -1;
  
#ifdef USE_MPI
  extern int blockProcessorMap[CMG_MAX_BLOCK_DEFINITIONS];
  extern MeshCellContainer cellContainer;
  MPI_Comm_rank(MPI_COMM_WORLD, &domainId);
  int cellId = getCellId(cellContainer.cellList[blockProcessorMap[domainId]].i,
                         cellContainer.cellList[blockProcessorMap[domainId]].j,
                         cellContainer.cellList[blockProcessorMap[domainId]].k);
  
  (*nnodes) = getMeshCellNodeSetSize(cellId,1);
  
#endif
}


/*!Fortran cnmnoda( )*/
void cnmnoda_(int *nnodes)
{
  cnmnoda(nnodes);
}


/*!Used in conjunction with cnmnoda,
  returns all node ids that touch or are in the domain,
  giving the offset of where in the array the non-owned
  nodes start
*/
void cnodsa(int *nodes, int *offset)
{
  int totalNodes;
  cnmnoda(&totalNodes);
  
  int myNodes;
  cnumnod(&myNodes);
  int size,jj,kk,counter;

  (*offset) = myNodes;
  counter = myNodes;
  
  
  cnods(nodes);
  

    /*If i get my nodes and my receive nodes I should have everything.*/
  
  crecnsz(&size);
  int *domainArray = (int*)malloc(sizeof(int) *size);
  int *nodeSizeArray = (int*)malloc(sizeof(int) * size);

  crecnar(domainArray,nodeSizeArray);
  for(jj=0;jj<size;++jj){
    int *nodeIds = (int*)malloc(sizeof(int) * nodeSizeArray[jj]);
    crecn(domainArray[jj],nodeIds);
    for(kk=0;kk<nodeSizeArray[jj];++kk){
      nodes[counter] = nodeIds[kk];
      counter++;
    }
    free(nodeIds);
  }

  free(domainArray);
  free(nodeSizeArray); 

}


/*!
  Fortran cnodsa( )
*/
void cnodsa_(int *nodes, int *offset)
{
  cnodsa(nodes,offset);
}

