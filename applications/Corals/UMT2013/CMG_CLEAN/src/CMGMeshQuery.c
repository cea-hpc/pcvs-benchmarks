/*
  // Copyright 2005 The Regents of the University of California.
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

  This file contains the implementaiton of the methods from cmg.h that
  will be used the query the mesh data structure.
*/
#include <assert.h>

#include "cmg.h"
#include "dataTypes.h"
#include "CMGDebug.h"
#include "meshAndInputData.h"
#include "CMGMeshTopology.h"
#include "CMGMeshQuery.h"
#include <stdlib.h>
#ifdef USE_MPI
#include "mpi.h"
#endif



/*!
  Get the node position for the given node
*/
void cnodpos (int nodeid, double *x, double *y, double *z) {

  extern MeshCellContainer cellContainer;
  extern MeshFaceContainer faceContainer;
  extern MeshEdgeContainer edgeContainer;
  extern MeshVertexContainer vertexContainer;
  extern SuperMeshSize sms;
  
  int i;
  
  
  int maxVertices = (sms.i+1)*(sms.j+1)*(sms.k+1);
  int maxEdges = ((sms.i)*(sms.j+1)*(sms.k+1))+
      ((sms.i+1)*sms.j*(sms.k+1))+
      ((sms.i+1)*(sms.j+1)*(sms.k));
  int maxFaces = ((sms.i+1)*sms.j*sms.k)+
      (sms.i*(sms.j+1)*sms.k)+
      (sms.i*sms.j*(sms.k+1));
 int maxCells = sms.i*sms.j*sms.k;

 for(i =0;i<maxVertices;++i){
   if(nodeid == vertexContainer.vertexList[i].nodeId){
     getNodePositionFromVertex(i,nodeid,x,y,z);
     return;
   }
 }
 for(i=0;i<maxEdges;++i){
   int base = edgeContainer.edgeList[i].baseNodeId;
   int numnodes = edgeContainer.edgeList[i].numNodes;
   if(nodeid>= base &&
      nodeid<(base+numnodes)){
     getNodePositionFromEdge(i,nodeid,x,y,z);
     return;
   }
 }
 for(i=0;i<maxFaces;++i){
   int base = faceContainer.faceList[i].baseNodeId;
   int numnodes = faceContainer.faceList[i].numNodes;
   if(nodeid>=base &&
      nodeid<(base+numnodes)){
     getNodePositionFromFace(i,nodeid,x,y,z);
     return;
   }
 }
 for(i=0;i<maxCells;++i){
   int base = cellContainer.cellList[i].baseNodeId;
   int numnodes = cellContainer.cellList[i].numNodes;
   if(nodeid>=base &&
      nodeid<(base+numnodes)){
     getNodePositionFromCell(i,nodeid,x,y,z);
     return;
   }
 }
}
/*! Fortran cnodpos() */
void cnodpos_(int *nodeid, double *x, double *y, double *z) {
  /* Call C cnodpos */
  cnodpos( (*nodeid), x, y, z );
}

/*!
  Get all the node positions for the domain.  The arguments are pointers
  to arrays that are same size as the number of nodes on the domain
*/
void cndspos(double *x, double *y, double *z)
{
    /*Here we will check to see if we have mpi, as for now
      we will return all the nodes in the problem.  When mpi
      is being used we will return the nodes on the rank of this processor*/
  
  int numNodes;
  int ii;
  double xp,yp,zp;
  int nodeId;
  

  cnumnod(&numNodes);
#ifdef USE_MPI
  int *nodeList = malloc(sizeof(int) * numNodes);
  cnods(nodeList);
#endif

  for(ii=0;ii<numNodes;++ii){
    #ifdef USE_MPI
    nodeId = nodeList[ii];
    #else
    nodeId = ii;
    #endif
    cnodpos(nodeId,&xp,&yp,&zp);
    x[ii] = xp;
    y[ii] = yp;
    z[ii] = zp;
  }

  #ifdef USE_MPI
  free(nodeList);
  #endif
  

}
/*! Fortran cnodpos() */
void cndspos_(double *x, double *y, double *z)
{
  cndspos(x,y,z);
}


/*!
  Get the number of nodes on this domain
*/
void cnumnod (int *nnodes) {

  #ifdef USE_MPI
  
  (*nnodes) = numDomainNodes( );
  
  #else
  extern SuperMeshSize sms;
  extern MeshCellContainer cellContainer;
  
  
  int maxCells = sms.i*sms.j*sms.k;

  int i;
  for(i = maxCells-1;i>=0;--i){
    if(cellContainer.cellList[i].i>-1){
      (*nnodes) = cellContainer.cellList[i].baseNodeId +
          cellContainer.cellList[i].numNodes;
      return;
    }
  }
  #endif
}
void cnods(int *nodes)
{
  #ifdef USE_MPI
  getDomainNodes(nodes);
  #else
  int numNodes;
  cnumnod(&numNodes);
  int ii;
  for(ii=0;ii<numNodes;++ii)
      nodes[ii] = ii;
  
  #endif
  

}

/*!Fortran cnods()
*/
void cnods_(int *nodes)
{
  cnods(nodes);
}




void cnumnod_(int *nnodes) {
  /* Call C cnumnod */

  cnumnod( nnodes );
}

/*!
  Get the number of faces on this domain
*/
void cnumfcs (int *nfaces) { assert( "Not implemented" ); }

void cnumfcs_(int *nfaces) {
  /*Call C */
  cnumfcs( nfaces );
}

/*!
  Get the number of zones on this domain
*/
void cnumzns (int *nzones) {
  
  int domainId = -1;

  #ifdef USE_MPI
  extern int blockProcessorMap[CMG_MAX_BLOCK_DEFINITIONS];
  extern MeshCellContainer cellContainer;
  
  MPI_Comm_rank(MPI_COMM_WORLD, &domainId);

  (*nzones) = cellContainer.cellList[blockProcessorMap[domainId]].numZones;
  #else
  extern ZoneDataContainer zoneData;
  (*nzones) = zoneData.numZones;
  #endif
  

 
  
}
void cnumzns_(int *nzones) { 
  /*Call C */
  cnumzns( nzones );
}

/*!
  Get the zones on this domain
*/
void czns( int *zones )
{
  int ii;
  
  #ifdef USE_MPI
  extern int blockProcessorMap[CMG_MAX_BLOCK_DEFINITIONS];
  extern MeshCellContainer cellContainer;
  int domainId;
  
  MPI_Comm_rank(MPI_COMM_WORLD, &domainId);
  int base = cellContainer.cellList[blockProcessorMap[domainId]].baseZoneId;
  int numZones = cellContainer.cellList[blockProcessorMap[domainId]].numZones;
  for(ii=0;ii<numZones;++ii)
      zones[ii] = base+ii;
  
  
  #else
  extern ZoneDataContainer zoneData;
  for(ii = 0;ii<zoneData.numZones;++ii)
      zones[ii] = ii;
  
  #endif  
  

}
void czns_( int *zones )
{
  czns(zones);
}


/*!
  Get the zone type for a given zone
  8-hex, 6-prism, 5-pyramid, 4-tetrahedron
*/
void cgetztp (int locid, cZoneType *typ) {

  extern ZoneDataContainer zoneData;
  if( locid >= zoneData.numZones )
    printf(" cgetztp, out of bounds array access. locid=%d, numZones=%d\n",locid,zoneData.numZones);
  
  (*typ) = (cZoneType)zoneData.zoneTypes[locid];
  

}
/*! Fortran cgetztp() */
void cgetztp_(int *locid, int *typ) { 
  /*Call C */
  cgetztp( (*locid), (cZoneType*)typ );
}

/*!
  Get the nodes in order for the given zone
*/

void cgetznn (int gloid, int *nodes) {
  
    /*first lets figure out where this is in the block*/
  
  extern MeshCellContainer cellContainer;
  extern SuperMeshSize sms;
  extern NumZones numZones;
  
  int maxCells = sms.i*sms.j*sms.k;
  int ii,i,j,k,cellId,localId;
  int subdivided = 0;
  int subdividedPosition;
  
  
  
  

  for(ii=0;ii<maxCells;++ii){
    int base = cellContainer.cellList[ii].baseZoneId;
    int offset = cellContainer.cellList[ii].numZones;
    if(gloid>=base &&
       gloid<(base+offset)){
      cellId = ii;
      break;
    }
  }

/*let's see if this zone is subdivided, this search is slow
      to make this faster it might not be a bad idea to have a 3d
      matrix that can be indexed by ijk, and return either one or zero.
      This would take a little more allocation overhead, but fast look-up.
      I am not going to implement this now because I don't know if it is needed.
    */
  localId = gloid - cellContainer.cellList[cellId].baseZoneId;

  if(localId >= cellContainer.cellList[cellId].numStructuredZones){
    localId = localId - cellContainer.cellList[cellId].numStructuredZones;
      /*These are are new zones, how many groupings of six are we above
        our structured ids.  this will give us the original zone id, so
        we can calculate node connectivity faster
      */
    subdividedPosition = localId/6;
    subdivided = 1;
      /*Get back the local id if the zone were unchanged*/
    localId = cellContainer.cellList[cellId].subdivisionIds[(localId/6)] -
        cellContainer.cellList[cellId].baseZoneId;
  }
  else{
    for(ii=0;ii<cellContainer.cellList[cellId].numberSubdivisions;++ii){
      int subdividedId = cellContainer.cellList[cellId].subdivisionIds[ii];
      if(gloid == subdividedId){
        subdivided = 1;
        subdividedPosition = ii;
        break;
      }
    }
  }
  
  
  
  

  int ni = numZones.iZones[cellContainer.cellList[cellId].i];
  int nj = numZones.jZones[cellContainer.cellList[cellId].j];
  int nk = numZones.kZones[cellContainer.cellList[cellId].k];

  
    
  i = (localId)/(nj*nk);
  j = ((localId) -(i*nj*nk))/nk;
  k = localId - (i*nj*nk) - (j*nk);
  
  if( subdivided == 0){
        /*return nodes in the following order
          (i,j,k),(i,j,k+1),(i+1,j,k+1),(i+1,j,k)
          (i,j+1,k),(i,j+1,k+1),(i+1,j+1,k+1),(i+1,j+1,k)
        */

      nodes[0] = getNodeFromIJK(i,j,k,cellId);
      nodes[1] = getNodeFromIJK(i,j,k+1,cellId);
      nodes[2] = getNodeFromIJK(i+1,j,k+1,cellId);
      nodes[3] = getNodeFromIJK(i+1,j,k,cellId);
      nodes[4] = getNodeFromIJK(i,j+1,k,cellId);
      nodes[5] = getNodeFromIJK(i,j+1,k+1,cellId);
      nodes[6] = getNodeFromIJK(i+1,j+1,k+1,cellId);
      nodes[7] = getNodeFromIJK(i+1,j+1,k,cellId);
    }
    else{
        /*Recalculate localId again so we have the real local id, not the
          undisturbed localId
        */
      localId = gloid - cellContainer.cellList[cellId].baseZoneId;
        /* If we are here we are in a zone that has been subdivided.
           We have two cases.  If the zone's id is less than the base+numStructuredZones,
           then this is the center zone in the subdivided zone.  If the id is higher
           than base+numStructuredZones then we are in one of the "wing" zones
        */
      int startId = cellContainer.cellList[cellId].baseNodeId +
            cellContainer.cellList[cellId].numStructuredNodes + (subdividedPosition * 8);
      
      if(localId < cellContainer.cellList[cellId].numStructuredZones){
          /*Return the center nodes*/
        
          /*We return the nodes in the same order, as we return the undisturbed zone.
            Above if we numbered the nodes on each zone locally we would return them in
            the following order (0,1,5,4,2,3,7,6)
          */
        nodes[0] = startId;
        nodes[1] = startId + 1;
        nodes[2] = startId + 5;
        nodes[3] = startId + 4;
        nodes[4] = startId + 2;
        nodes[5] = startId + 3;
        nodes[6] = startId + 7;
        nodes[7] = startId + 6;
        
      }
      else{
          /*This is a "wing" zone, it includes 4 of our new nodes and 4 of our existing nodes
           We have our start Id up above, and we have our ijk above. We have everything we need
          */

        switch( ((localId - cellContainer.cellList[cellId].numStructuredZones) %6) ){
          case 0: /* -k wing zone */
          {
            nodes[0] = getNodeFromIJK(i,j,k,cellId);
            nodes[1] = startId;
            nodes[2] = startId + 4;
            nodes[3] = getNodeFromIJK(i+1,j,k,cellId);
            nodes[4] = getNodeFromIJK(i,j+1,k,cellId);
            nodes[5] = startId + 2;
            nodes[6] = startId + 6;
            nodes[7] = getNodeFromIJK(i+1,j+1,k,cellId);
            break;
          }
          case 1:/* +k wing zone */
          {
            nodes[0] = startId + 1;
            nodes[1] = getNodeFromIJK(i,j,k+1,cellId);
            nodes[2] = getNodeFromIJK(i+1,j,k+1,cellId);
            nodes[3] = startId + 5;
            nodes[4] = startId + 3;
            nodes[5] = getNodeFromIJK(i,j+1,k+1,cellId);
            nodes[6] = getNodeFromIJK(i+1,j+1,k+1,cellId);
            nodes[7] = startId + 7;
            break;
          }
          case 2:/* -j wing zone */
          {
            nodes[0] = getNodeFromIJK(i,j,k,cellId);
            nodes[1] = getNodeFromIJK(i,j,k+1,cellId);
            nodes[2] = getNodeFromIJK(i+1,j,k+1,cellId);
            nodes[3] = getNodeFromIJK(i+1,j,k,cellId);
            nodes[4] = startId;
            nodes[5] = startId + 1;
            nodes[6] = startId + 5;
            nodes[7] = startId + 4;
            break;
          }
          case 3: /* +j wing zone */
          {
            nodes[0] = startId + 2;
            nodes[1] = startId + 3;
            nodes[2] = startId + 7;
            nodes[3] = startId + 6;
            nodes[4] = getNodeFromIJK(i,j+1,k,cellId);
            nodes[5] = getNodeFromIJK(i,j+1,k+1,cellId);
            nodes[6] = getNodeFromIJK(i+1,j+1,k+1,cellId);
            nodes[7] = getNodeFromIJK(i+1,j+1,k,cellId);
            break;
          }
          case 4: /* -i wing zone */
          {
           nodes[0] = getNodeFromIJK(i,j,k,cellId);
           nodes[1] = getNodeFromIJK(i,j,k+1,cellId);
           nodes[2] = startId + 1;
           nodes[3] = startId;
           nodes[4] = getNodeFromIJK(i,j+1,k,cellId);
           nodes[5] = getNodeFromIJK(i,j+1,k+1,cellId);
           nodes[6] = startId + 3;
           nodes[7] = startId + 2;
           break;
          }
          case 5: /* +i wing zone */
          {
            nodes[0] = startId + 4;
            nodes[1] = startId + 5;
            nodes[2] = getNodeFromIJK(i+1,j,k+1,cellId);
            nodes[3] = getNodeFromIJK(i+1,j,k,cellId);
            nodes[4] = startId + 6;
            nodes[5] = startId + 7;
            nodes[6] = getNodeFromIJK(i+1,j+1,k+1,cellId);
            nodes[7] = getNodeFromIJK(i+1,j+1,k,cellId);
            break;
          }
          default:
              CMGDPRINT("Incorrect case in CMGMeshQuery::getznn report to grover5@llnl.gov\n");
        }
      } 
    }
  
}

/*! Fortran cgetznn() */
void cgetznn_(int *locid, int *nodes) { 
  /*Call C */
  cgetznn( (*locid), nodes );
}

int getNodeFromIJK( int i, int j, int k, int cellId)
{
  extern MeshCellContainer cellContainer;
  extern SuperMeshSize sms;
  extern NumZones numZones;

  int cellI = cellContainer.cellList[cellId].i;
  int cellJ = cellContainer.cellList[cellId].j;
  int cellK = cellContainer.cellList[cellId].k;

  int iNodes = numZones.iZones[cellI] + 1;
  int jNodes = numZones.jZones[cellJ] + 1;
  int kNodes = numZones.kZones[cellK] + 1;

    /*If the node is on a face then the ij for that face will
      always have the same i or j of the cell, for example if i==0
      then the face is a face that has jk for its indicies, j will
      remain j and k will become i.  It is the same for all other faces
    */

  if(i == 0)
      return getNodeFromIJ(k,j,cellContainer.cellList[cellId].meshFaceIds[0]);
  else if(i == iNodes-1)
      return getNodeFromIJ(k,j,cellContainer.cellList[cellId].meshFaceIds[1]);
  else if(j == 0)
      return getNodeFromIJ(i,k,cellContainer.cellList[cellId].meshFaceIds[2]);
  else if(j == jNodes -1)
      return getNodeFromIJ(i,k,cellContainer.cellList[cellId].meshFaceIds[3]);
  else if(k == 0)
      return getNodeFromIJ(i,j,cellContainer.cellList[cellId].meshFaceIds[4]);
  else if(k == kNodes -1)
      return getNodeFromIJ(i,j,cellContainer.cellList[cellId].meshFaceIds[5]);
  else{
      /*This node is interior to this cell*/
      /*nodes in the cell start at 1,1,1, so we need
        to translate the ijk by 1 in each direction to get the node id,
        we also need to subtract 2 from the number of nodes since we are looking
        at none of the nodes on the boundary*/
    return ((kNodes-2)*(jNodes-2)*(i-1)) +((kNodes-2)*(j-1)) +(k-1) +
        cellContainer.cellList[cellId].baseNodeId;
  }
}

int getNodeFromIJ(int i, int j, int faceId)
{
  extern MeshFaceContainer faceContainer;
  extern SuperMeshSize sms;
  extern NumZones numZones;
  
  int alignment = faceContainer.faceList[faceId].alignment;
  int faceI = faceContainer.faceList[faceId].i;
  int faceJ = faceContainer.faceList[faceId].j;
  int faceK = faceContainer.faceList[faceId].k;
  
  
  if(alignment == 0){
      /*this is an i normal face, i maps to k*/
    int iNodes = numZones.kZones[faceK] +1;
    int jNodes = numZones.jZones[faceJ] +1;
    if(i == 0)
        return getNodeFromI(j,faceContainer.faceList[faceId].meshEdgeIds[0]);
    else if(i==iNodes-1)
        return getNodeFromI(j,faceContainer.faceList[faceId].meshEdgeIds[2]);
    else if(j == 0)
        return getNodeFromI(i,faceContainer.faceList[faceId].meshEdgeIds[3]);
    else if(j == jNodes-1)
        return getNodeFromI(i,faceContainer.faceList[faceId].meshEdgeIds[1]);
    else{
        /*this is interior to the face, since nodes start at 1,1
          this will have the same behavior as the cell, so we will
          offset by 1 in both the i and j directions*/
      return((jNodes-2)*(i-1) + (j-1)) + faceContainer.faceList[faceId].baseNodeId;  
    }
  }
  else if(alignment == 1){
      /*this is a j normal face k maps to j*/
    int iNodes = numZones.iZones[faceI] + 1;
    int jNodes = numZones.kZones[faceK] + 1;
    if(i==0)
        return getNodeFromI(j,faceContainer.faceList[faceId].meshEdgeIds[1]);
    else if(i == iNodes-1)
        return getNodeFromI(j,faceContainer.faceList[faceId].meshEdgeIds[3]);
    else if(j==0)
        return getNodeFromI(i,faceContainer.faceList[faceId].meshEdgeIds[0]);
    else if(j == jNodes-1)
        return getNodeFromI(i,faceContainer.faceList[faceId].meshEdgeIds[2]);
    else{
        /*this is interior to the face, since nodes start at 1,1, we will
          have to remap must like on the i faces*/
      return ((jNodes-2)*(i-1) + (j-1)) + faceContainer.faceList[faceId].baseNodeId;
    }
  }
  else if(alignment == 2){
      /*this is a k normal face i and j are as is*/
    int iNodes = numZones.iZones[faceI] + 1;
    int jNodes = numZones.jZones[faceJ] + 1;
    if(i==0)
        return getNodeFromI(j,faceContainer.faceList[faceId].meshEdgeIds[0]);
    else if(i == iNodes-1)
        return getNodeFromI(j,faceContainer.faceList[faceId].meshEdgeIds[2]);
    else if(j == 0)
        return getNodeFromI(i,faceContainer.faceList[faceId].meshEdgeIds[1]);
    else if(j == jNodes-1)
        return getNodeFromI(i,faceContainer.faceList[faceId].meshEdgeIds[3]);
    else{
        /*see comments above*/
      return ((jNodes-2)*(i-1) + (j-1)) + faceContainer.faceList[faceId].baseNodeId;
    }
  }
  else{
    assert(1);
  }
  return;
  
}
int getNodeFromI(int i, int edgeId)
{
  extern MeshEdgeContainer edgeContainer;
  extern MeshVertexContainer vertexContainer;
  
  if(i==0)
      return vertexContainer.vertexList[edgeContainer.edgeList[edgeId].meshVertexIds[0]].nodeId;
  else if(i == (1+ edgeContainer.edgeList[edgeId].numNodes))
      return vertexContainer.vertexList[edgeContainer.edgeList[edgeId].meshVertexIds[1]].nodeId;
  else
    return (i-1) + edgeContainer.edgeList[edgeId].baseNodeId;
}
void getNodePositionFromCell(int cellId,int nodeId,double *x, double *y, double *z)
{
  extern SuperMeshSize sms;
  extern NumZones numZones;
  extern MeshCellContainer cellContainer;

  

  double xstep = 1.0/(double)sms.i;
  double ystep = 1.0/(double)sms.j;
  double zstep = 1.0/(double)sms.k;

  int celli = cellContainer.cellList[cellId].i;
  int cellj = cellContainer.cellList[cellId].j;
  int cellk = cellContainer.cellList[cellId].k;

  double xstart = (double)celli * xstep;
  double ystart = (double)cellj * ystep;
  double zstart = (double)cellk * zstep;

  int numIDivisions = numZones.iZones[celli];
  int numJDivisions = numZones.jZones[cellj];
  int numKDivisions = numZones.kZones[cellk];

  double subIStep = xstep/numIDivisions;
  double subJStep = ystep/numJDivisions;
  double subKStep = zstep/numKDivisions;
  
  int nodeOffset = nodeId - cellContainer.cellList[cellId].baseNodeId;

  if(nodeOffset < cellContainer.cellList[cellId].numStructuredNodes){
    int localI = nodeOffset/((numJDivisions-1)*(numKDivisions-1));
    int localJ = ((nodeOffset)-(localI*(numJDivisions-1)*(numKDivisions-1)))/(numKDivisions-1);
    int localK = nodeOffset - (localI*(numJDivisions-1)*(numKDivisions-1)) - (localJ*(numKDivisions-1));
  
    (*x) = xstart + ((double)(localI+1)*subIStep);
    (*y) = ystart + ((double)(localJ+1)*subJStep);
    (*z) = zstart + ((double)(localK+1)*subKStep);
  }
  else{
      /*Basically we want to find which original structured zone this node is part of.
        Since we are in this else it is not one of the corners of the big original
        unstrcutured zone, so it has to be an interior node of the big unstrcutred
        zone, i.e. the nodes of the embedded hex in this hex. We figure out which
        hex it is in, find the starting positions of the of the ijk node of the big
        hex, then we try and find which of 8 nodes this is on the new smaller imbedded
        hex.  If we look at that smaller imbedded hex, it has a local ijk, going from 0-1 in
        each direction.  If we find that IJK we can then find the locations.
      */
    nodeOffset = nodeOffset - cellContainer.cellList[cellId].numStructuredNodes;

    int zoneOffset = nodeOffset/8;
    if(zoneOffset>= cellContainer.cellList[cellId].numberSubdivisions)
        CMGDPRINT("Error in getNodePositionFromCell, report to grover5@llnl.gov\n");
    
    int zoneId = cellContainer.cellList[cellId].subdivisionIds[zoneOffset];
    int localZoneId = zoneId - cellContainer.cellList[cellId].baseZoneId;

    int localZoneI = localZoneId/((numJDivisions)*(numKDivisions));
    int localZoneJ = ((localZoneId)-(localZoneI*(numJDivisions)*(numKDivisions)))/(numKDivisions);
    int localZoneK = localZoneId - (localZoneI*(numJDivisions)*(numKDivisions)) - (localZoneJ*(numKDivisions));

      /*This is the i,j,k corner of this zone.*/
    xstart = xstart + (localZoneI * subIStep);
    ystart = ystart + (localZoneJ * subJStep);
    zstart = zstart + (localZoneK * subKStep);
    
    nodeOffset = nodeOffset%8; /*0-7*/
    double smallOffset[2] = {.25,.75};
      /*The node offset is essentially it's local id in ***This*** zone*/
      /*We need to find wheter we are at 0 or 1 in the very local ijk space*/
      /*The method above simplifies, becasue numI=numJ=numK = 2*/
    int localI = nodeOffset / 4;
    int localJ = (nodeOffset -(localI*4))/2;
    int localK = nodeOffset - (localI*4) - (localJ*2);

    (*x) = xstart + (subIStep*smallOffset[localI]);
    (*y) = ystart + (subJStep*smallOffset[localJ]);
    (*z) = zstart + (subKStep*smallOffset[localK]);
  }
}

void getNodePositionFromFace(int faceId, int nodeId,double *x, double *y, double *z)
{
  extern SuperMeshSize sms;
  extern MeshFaceContainer faceContainer;
  extern NumZones numZones;
  double istep,jstep;
  double subIStep,subJStep;
  int numIDivisions,numJDivisions;
  int localI,localJ;
  int facei,facej,nodeOffset;
  
  double xstep = 1.0/(double)sms.i;
  double ystep = 1.0/(double)sms.j;
  double zstep = 1.0/(double)sms.k;
  
  if(faceContainer.faceList[faceId].alignment ==0){
      /*i normal face, x remains constant*/
      /*on an i normal face, j remains j and k becomes i*/
    (*x) = (double)faceContainer.faceList[faceId].i*xstep;

    facei = faceContainer.faceList[faceId].k;
    facej = faceContainer.faceList[faceId].j;
    istep = zstep;
    jstep = ystep;

    double ystart = (double)facej*jstep;
    double zstart = (double)facei*istep;

    numJDivisions = numZones.jZones[facej];
    numIDivisions = numZones.kZones[facei];

    subIStep = istep/(double)numIDivisions;
    subJStep = jstep/(double)numJDivisions;

    nodeOffset = nodeId - faceContainer.faceList[faceId].baseNodeId;

      /*with the offset, we need to remember that we start counting
        nodes on the interior of the face, so if this is node 0,
        in the space on the face it is really at 1,1, not 0,0*/

      /*number of divisions is the number of zones, since we are only
        looking at interior nodes, then there is one less interior
        node than division*/
    localI = nodeOffset/(numJDivisions-1);
    localJ = nodeOffset - (localI*(numJDivisions-1));

      /*add one back into local I and J to compensate for starting
        at 1,1*/
    (*y) = ystart + ((double)(localJ+1)*subJStep);
    (*z) = zstart + ((double)(localI+1)*subIStep);
    
  }
  else if(faceContainer.faceList[faceId].alignment ==1){
      /*j normal face y remains constant*/
      /*on a j normal face i remains i, and k becomes j*/

    (*y) = (double)faceContainer.faceList[faceId].j*ystep;

    facei = faceContainer.faceList[faceId].i;
    facej = faceContainer.faceList[faceId].k;

    istep = xstep;
    jstep = zstep;

    double xstart = (double)facei*istep;
    double zstart = (double)facej*jstep;

    numIDivisions = numZones.iZones[facei];
    numJDivisions = numZones.kZones[facej];

    subIStep = istep/(double)numIDivisions;
    subJStep = jstep/(double)numJDivisions;
    

    nodeOffset = nodeId - faceContainer.faceList[faceId].baseNodeId;

    localI = nodeOffset/(numJDivisions-1);
    localJ = nodeOffset - (localI*(numJDivisions-1));

    (*x) = xstart + ((double)(localI+1)*subIStep);
    (*z) = zstart + ((double)(localJ+1)*subJStep);
  }
  else if(faceContainer.faceList[faceId].alignment ==2){
      /*k normal face z remains constant*/
      /*on a k normal face i and j remain i and j*/

    (*z) = (double)faceContainer.faceList[faceId].k*zstep;

    facei = faceContainer.faceList[faceId].i;
    facej = faceContainer.faceList[faceId].j;

    istep = xstep;
    jstep = ystep;

    double xstart = (double)facei*istep;
    double ystart = (double)facej*jstep;

    numIDivisions = numZones.iZones[facei];
    numJDivisions = numZones.jZones[facej];

    subIStep = istep/(double)numIDivisions;
    subJStep = jstep/(double)numJDivisions;

    nodeOffset = nodeId - faceContainer.faceList[faceId].baseNodeId;

    localI = nodeOffset/(numJDivisions-1);
    localJ = nodeOffset - (localI*(numJDivisions-1));

    (*x) = xstart + ((double)(localI+1) * subIStep);
    (*y) = ystart + ((double)(localJ+1) * subJStep);
    
  }
}

void getNodePositionFromEdge(int edgeId, int nodeId, double *x, double *y, double *z)
{
  extern SuperMeshSize sms;
  extern MeshEdgeContainer edgeContainer;
  extern NumZones numZones;
  double xstep = 1.0/(double)sms.i;
  double ystep = 1.0/(double)sms.j;
  double zstep = 1.0/(double)sms.k;

    /*all edges are aligned, that means that all i edges flow from -i to +i
      the same is true for j an k*/
  if(edgeContainer.edgeList[edgeId].alignment ==0){
      /*i aligned edge, the x will change*/
    (*y) = (double)edgeContainer.edgeList[edgeId].j*ystep;
    (*z) = (double)edgeContainer.edgeList[edgeId].k*zstep;
      /*Now let's figure out x*/

    int edgei = edgeContainer.edgeList[edgeId].i;
    double istep = 1.0/(double)sms.i;

    double xstart = (double)edgei * istep;

      /*figure out the number of divisions on this one sub-block
       */
    int numDivisions = numZones.iZones[edgei];
    double subStep = istep/(double)numDivisions;

      /*must add one to the nodeOffset since to account for
        the vertices at both ends*/
    int nodeOffset = nodeId - edgeContainer.edgeList[edgeId].baseNodeId +1;

    (*x) = xstart + ((double)nodeOffset*subStep); 
  }
  else if(edgeContainer.edgeList[edgeId].alignment ==1){
      /*j aligned edge, the y will change*/
    
    (*x) = (double)edgeContainer.edgeList[edgeId].i*xstep;
    (*z) = (double)edgeContainer.edgeList[edgeId].k*zstep;

    int edgej = edgeContainer.edgeList[edgeId].j;
    double jstep = 1.0/(double)sms.j;

    double ystart = (double)edgej * jstep;

    int numDivisions = numZones.jZones[edgej];
    double subStep = jstep/(double)numDivisions;

    int nodeOffset = nodeId  - edgeContainer.edgeList[edgeId].baseNodeId + 1;
    (*y) = ystart +((double)nodeOffset*subStep);
    
  }
  else if(edgeContainer.edgeList[edgeId].alignment ==2){
      /*k aligned edge, the z will change*/
    (*x) = (double)edgeContainer.edgeList[edgeId].i*xstep;
    (*y) = (double)edgeContainer.edgeList[edgeId].j*ystep;

    int edgek = edgeContainer.edgeList[edgeId].k;
    double kstep = 1.0/(double)sms.k;

    double zstart = (double)edgek * kstep;

    int numDivisions = numZones.kZones[edgek];
    double subStep = kstep/(double)numDivisions;

    int nodeOffset = nodeId - edgeContainer.edgeList[edgeId].baseNodeId + 1;
    (*z) = zstart + ((double)nodeOffset*subStep);
    
    
  }

}

void getNodePositionFromVertex(int vertexId, int nodeId,double *x, double *y, double *z)
{
  extern MeshVertexContainer vertexContainer;
  extern SuperMeshSize sms;

  double xstep = 1.0/(double)sms.i;
  double ystep = 1.0/(double)sms.j;
  double zstep = 1.0/(double)sms.k;

  (*x) = (double)vertexContainer.vertexList[vertexId].i*xstep;
  (*y) = (double)vertexContainer.vertexList[vertexId].j*ystep;
  (*z) = (double)vertexContainer.vertexList[vertexId].k*zstep;

}




