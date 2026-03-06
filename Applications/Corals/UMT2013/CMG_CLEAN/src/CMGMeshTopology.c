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
// Benjamin T. Grover and Walter Nissen

This file contains the structures and accessor functions for the lightweight mesh
topology that will be used to construct the mesh.
*/

#include "CMGMeshTopology.h"
#include "CMGDebug.h"
#include "meshAndInputData.h"
#include <stdlib.h>

#ifdef USE_MPI
#include "mpi.h"
#endif

MeshCellContainer cellContainer;

MeshFaceContainer faceContainer;

MeshEdgeContainer edgeContainer;

MeshVertexContainer vertexContainer;

void MeshVertexInit( MeshVertex *this)
{
  this->i = -1;
  this->j = -1;
  this->k = -1;
  this->nodeId = -1;
}

void MeshEdgeInit( MeshEdge *this)
{
  this->i = -1;
  this->j = -1;
  this->k = -1;
  this->meshVertexIds[0] = -1;
  this->meshVertexIds[1] = -1;
  this->baseNodeId = -1;
  this->numNodes = -1;
  this->baseEdgeId = -1;
  this->numEdges = -1;
  
}

void MeshFaceInit( MeshFace *this)
{
  this->i = -1;
  this->j = -1;
  this->k = -1;
  this->baseNodeId = -1;
  this->numNodes = -1;
  this->baseEdgeId = -1;
  this->numEdges = -1;
  this->baseFaceId = -1;
  this->numFaces = -1;
  this->alignment = -1;
  this->meshEdgeIds[0] = -1;
  this->meshEdgeIds[1] = -1;
  this->meshEdgeIds[2] = -1;
  this->meshEdgeIds[3] = -1;
}

void MeshCellInit(MeshCell *this)
{
  this->i = -1;
  this->j = -1;
  this->k = -1;
  this->meshFaceIds[0] = -1;
  this->meshFaceIds[1] = -1;
  this->meshFaceIds[2] = -1;
  this->meshFaceIds[3] = -1;
  this->meshFaceIds[4] = -1;
  this->meshFaceIds[5] = -1;
  this->baseZoneId = -1;
  this->numZones = -1;
  this->numStructuredZones = -1;
  this->baseNodeId = -1;
  this->numNodes = -1;
  this->numStructuredNodes = -1;
  this->baseFaceId = -1;
  this->numFaces =-1;
  this->baseEdgeId = -1;
  this->numEdges = -1;

  this->numberSubdivisions = 0;
  this->subdivisionIds = 0;
  
}



void MeshFaceContainerInit( MeshFaceContainer *this)
{
  int i;
  for(i=0;i<CMG_MAX_FACE_DEFINITIONS;++i)
      MeshFaceInit(&(this->faceList[i]));
}



void MeshVertexContainerInit( MeshVertexContainer *this)
{
  int i;
  for(i=0;i<CMG_MAX_VERTEX_DEFINITIONS;++i)
      MeshVertexInit(&(this->vertexList[i]));
  
}

void MeshEdgeContainerInit( MeshEdgeContainer *this)
{
  int i;
  for(i=0;i<CMG_MAX_EDGE_DEFINITIONS;++i)
      MeshEdgeInit(&(this->edgeList[i]));
  
}

void MeshCellContainerInit( MeshCellContainer *this)
{
  int i;
  for(i=0;i<CMG_MAX_BLOCK_DEFINITIONS;++i)
      MeshCellInit(&(this->cellList[i]));
  
} 


void MeshVertexPrint( const MeshVertex *this)
{
  if(this->i > -1)
      CMGDPRINT("MeshVertex i:%d j:%d k:%d, nodeId:%d\n",this->i,this->j,this->k,this->nodeId);
  else{
    CMGDPRINT(".\n");
  }
  
  
  
}

void MeshVertexContainerPrint( const MeshVertexContainer *this)
{
 
  int maxVertices = getMaxVertices( );
  
  
  int i;
  for(i = 0; i<maxVertices;++i)
      MeshVertexPrint(&(this->vertexList[i]));
}


void MeshEdgePrint( const MeshEdge *this)
{
  if(this->i >-1)
      CMGDPRINT("MeshEdge i:%d j:%d k:%d alignment:%d\n\tbaseNodeId:%d numNodes:%d\n",this->i,this->j,this->k,this->alignment,this->baseNodeId,this->numNodes);
  else{
    CMGDPRINT(".\n");
  }
}

void MeshEdgeContainerPrint( const MeshEdgeContainer *this)
{
 
  int maxEdges = getMaxEdges( );
  
  
  int i;
  for(i=0;i<maxEdges;++i)
      MeshEdgePrint(&(this->edgeList[i]));
}

void MeshFacePrint( const MeshFace *this)
{
  if(this->i>-1)
      CMGDPRINT("MeshFace i:%d j:%d k:%d alignment:%d\n\tbaseNodeId:%d numNodes:%d\n",this->i,this->j,this->k,this->alignment,this->baseNodeId,this->numNodes);
  else{
    CMGDPRINT(".\n");
  }
}

void MeshFaceContainerPrint( const MeshFaceContainer *this)
{
  
  int maxFaces = getMaxFaces( );
  
  int i;
  for(i=0;i<maxFaces;++i)
      MeshFacePrint(&(this->faceList[i]));
}

void MeshCellPrint( const MeshCell *this)
{
  if(this->i>-1)
      CMGDPRINT("MeshCell i:%d j:%d k:%d\n\tbaseNodeId:%d numNodes:%d baseZoneId:%d numZones:%d\n",this->i,this->j,this->k,this->baseNodeId,this->numNodes,
                this->baseZoneId,this->numZones);
  else
      CMGDPRINT(".\n");
}

void MeshCellContainerPrint( const MeshCellContainer *this)
{
 
  int maxCells = getMaxCells( );
  
  int i;
  for(i=0;i<maxCells;++i)
      MeshCellPrint(&(this->cellList[i]));
}

void createMeshTopology( )
{
  extern SuperMeshSize sms;
  int masterArraySize = sms.i*sms.j*sms.k;
  int *masterBlockArray = malloc(sizeof(int)*masterArraySize);
  calculateMasterBlockArray(masterBlockArray);
  int i,j,k,ii,jj,kk;
  int currentBlock;
 
   
  for(currentBlock = 0;currentBlock<masterArraySize;++currentBlock){
    if(masterBlockArray[currentBlock] == 1){
      CMGDPRINT("BlockId: %d\n",currentBlock);
      
      i = (currentBlock)/( (sms.j)* (sms.k));
      j = ((currentBlock)-(i*(sms.j)*(sms.k)))/(sms.k);
      k = (currentBlock) -(i*(sms.j)*(sms.k)) - (j*(sms.k));

      createMeshCellVertices(i,j,k);
      createMeshCellEdges(i,j,k);
      createMeshCellFaces(i,j,k);
      createMeshCell(i,j,k);
      
    }
  }
  free(masterBlockArray);
  
   
}
void createMeshCell( int i, int j, int k)
{
  extern SuperMeshSize sms;

    /*cells are straight forward,
      they are numbered moving in k first then j then i.
      cellid = nk*nj*i + nk*j + k
      faces are associated with the cell, in this order
      i,i+1,j,j+1,k,k+1
    */
  int inormalfaces = (sms.i+1) *sms.j*sms.k;
  int jnormalfaces = (sms.j+1) *sms.i*sms.k;

  int cellId = (sms.k*sms.j*i) + (sms.k * j) + k;

  cellContainer.cellList[cellId].i = i;
  cellContainer.cellList[cellId].j = j;
  cellContainer.cellList[cellId].k = k;

  int ii,jj,kk;
  int faceId;
 

  for(ii=0;ii<2;++ii){
      /*find i normal face ids*/
    faceId = (sms.k*sms.j*(i+ii))+(sms.k*j)+k;
    cellContainer.cellList[cellId].meshFaceIds[ii] = faceId;
  }
  for(jj=0;jj<2;++jj){
      /*find j normal face ids*/
    faceId = (sms.k*(sms.j+1)*i)+(sms.k*(j+jj))+k + inormalfaces;
    cellContainer.cellList[cellId].meshFaceIds[2+jj] = faceId;
  }
  for(kk=0;kk<2;++kk){
      /*find k normal face ids*/
    faceId = ((sms.k+1)*(sms.j)*i)+((sms.k+1)*j)+(k+kk) + inormalfaces + jnormalfaces;
    cellContainer.cellList[cellId].meshFaceIds[4+kk] = faceId;
  }
}

void createMeshCellFaces(int i, int j, int k)
{

    /*Faces are similar to edges in that they store an ijk, as well
      as an alignment so we know what "type" of edge it is.
      If a face has a 0 aligment that means it normal points in the i direction,
      1 normal points in j, and 2 normal points in k

      inormalfaces = (nk*nj*i) +(nk*j) + k
      jnormalfaces = (nk*(nj+1)*i) +(nk*j) + k + inormalfaces
      knormalfaces = ((nk+1)*nj*i) +((nk+1)*j) + k + inormalfaces + jnormalfaces

      edges are always ordered counter clockwise standing on the inside on the - face
      and on the outside on the + face always starting at the ijk edge, or one parallel to it
    */

  extern SuperMeshSize sms;
  int ii,jj,kk;
  
  int iedges = sms.i * (sms.j+1) *(sms.k+1);
  int jedges = (sms.i+1) *sms.j*(sms.k+1);
  
  int inormalfaces = (sms.i+1) *sms.j*sms.k;
  int jnormalfaces = (sms.j+1) *sms.i*sms.k;
  int faceId;
  for(ii=0;ii<2;++ii){
    faceId = (sms.k*sms.j*(i+ii))+(sms.k*j)+k;
    faceContainer.faceList[faceId].alignment = 0;
    faceContainer.faceList[faceId].i = i+ii;
    faceContainer.faceList[faceId].j = j;
    faceContainer.faceList[faceId].k = k;
      /*jedge*/
    faceContainer.faceList[faceId].meshEdgeIds[0] = ((sms.k+1)*(sms.j)*(i+ii))+
        ((sms.k+1)*j) +k + iedges;
      /*kedge*/
    faceContainer.faceList[faceId].meshEdgeIds[1] = ((sms.k)*(sms.j+1)*(i+ii))+
        ((sms.k)*(j+1)) + k + iedges + jedges;
      /*jedge*/
    faceContainer.faceList[faceId].meshEdgeIds[2] = ((sms.k+1)*(sms.j)*(i+ii))+
        ((sms.k+1)*j) +(k+1) + iedges;
      /*kedge*/
    faceContainer.faceList[faceId].meshEdgeIds[3] = ((sms.k)*(sms.j+1)*(i+ii))+
        ((sms.k)*(j)) + k + iedges + jedges;
  }

  for(jj=0;jj<2;++jj){
    faceId = (sms.k*(sms.j+1)*i)+(sms.k*(j+jj))+k + inormalfaces;
    faceContainer.faceList[faceId].alignment = 1;
    faceContainer.faceList[faceId].i = i;
    faceContainer.faceList[faceId].j = j+jj;
    faceContainer.faceList[faceId].k = k;

      /*iedge*/
    faceContainer.faceList[faceId].meshEdgeIds[0]=((sms.k+1)*(sms.j+1)*i) +
        ((sms.k+1)*(j+jj)) + k;
      /*kedge*/
    faceContainer.faceList[faceId].meshEdgeIds[1]=((sms.k)*(sms.j+1)*i) +
        ((sms.k)*(j+jj)) + k + iedges + jedges;
      /*iedge*/
    faceContainer.faceList[faceId].meshEdgeIds[2]=((sms.k+1)*(sms.j+1)*i) +
        ((sms.k+1)*(j+jj)) + k +1;
      /*kedge*/
    faceContainer.faceList[faceId].meshEdgeIds[3]=((sms.k)*(sms.j+1)*(i+1)) +
        ((sms.k)*(j+jj)) + k + iedges + jedges;;
  }

  for(kk=0;kk<2;++kk){
    faceId = ((sms.k+1)*(sms.j)*i)+((sms.k+1)*j)+(k+kk) + inormalfaces + jnormalfaces;
    faceContainer.faceList[faceId].alignment = 2;
    faceContainer.faceList[faceId].i = i;
    faceContainer.faceList[faceId].j = j;
    faceContainer.faceList[faceId].k = k+kk;
      /*jedge*/
    faceContainer.faceList[faceId].meshEdgeIds[0] =((sms.k+1) * sms.j * i) +
        ((sms.k+1)* j) + (k + kk) + iedges;
      /*iedge*/
    faceContainer.faceList[faceId].meshEdgeIds[1] =((sms.k+1)*(sms.j+1)*i) +
        ((sms.k+1)*j) + (k+kk);
      /*jedge*/
    faceContainer.faceList[faceId].meshEdgeIds[2] =((sms.k+1) * sms.j * (i+1)) +
        ((sms.k+1)* j) + (k + kk) + iedges;
      /*iedge*/
    faceContainer.faceList[faceId].meshEdgeIds[3] =((sms.k+1)*(sms.j+1)*i) +
        ((sms.k+1)*(j+1)) + (k+kk);
  }
        
}

void createMeshCellEdges(int i, int j, int k )
{
  
  extern SuperMeshSize sms;
  
  
  int ii,jj,kk;

    /*Edges not only have to store their ijk, but they also have to store their
      sense.  Edges are also special because each direction of edge has a different
      ijk.  For example edges that are paralell to i, have an i interval equal to that of
      i interval of sms, but the j and k intervals for the i aligned edges have j+1 and k+1
      intervals compared to the sms, because i aligned edges meet up at nodes in the j and k
      directions.

      
      i edges: (nk+1)*(nj+1)*i + (nk+1)*j + k
      j edges: (nk+1)*nj*i + (nk+1)*j + k + iedges
      k edges: nk*(nj+1)*i + nk*j + k + iedges + jedges

      This way we still have a deterministic way of labeling edges, but we wont
      confuse their ids. Label i edges first, then j edges and lastly k edges;
    */

  int iedges = sms.i * (sms.j+1) *(sms.k+1);
  int jedges = (sms.i+1) *sms.j*(sms.k+1);
  int edgeId;

    /*create i edges, i remains constant*/
  for(jj=0;jj<2;++jj){
    for(kk=0;kk<2;++kk){
      edgeId = ((sms.k+1)*(sms.j+1)*i) + ((sms.k+1)*(j+jj)) +(k+kk);
      edgeContainer.edgeList[edgeId].alignment = 0;
      edgeContainer.edgeList[edgeId].i = i;
      edgeContainer.edgeList[edgeId].j = j+jj;
      edgeContainer.edgeList[edgeId].k = k+kk;
        /*the vertices for the edge have the same i,j,k as the edge
          only differing by i+1
        */
      edgeContainer.edgeList[edgeId].meshVertexIds[0] =
          (sms.k+1)*(sms.j+1)*i + (sms.k+1)*(j+jj) + (k +kk);
      edgeContainer.edgeList[edgeId].meshVertexIds[1] =
          (sms.k+1)*(sms.j+1)*(i+1) + (sms.k+1)*(j+jj) + (k +kk);  
    }
  }
  
    /*now crerate the j edges, j remains constant*/
  for(ii=0;ii<2;++ii){
    for(kk=0;kk<2;++kk){
      edgeId = ((sms.k+1)*sms.j*(i+ii)) + ((sms.k+1)*j) + (k +kk) + iedges;
      edgeContainer.edgeList[edgeId].alignment = 1;
      edgeContainer.edgeList[edgeId].i = i +ii;
      edgeContainer.edgeList[edgeId].j = j;
      edgeContainer.edgeList[edgeId].k = k+kk;
      edgeContainer.edgeList[edgeId].meshVertexIds[0]=
          (sms.k+1)*(sms.j+1)*(i+ii) + (sms.k+1)*j + (k+kk);
      edgeContainer.edgeList[edgeId].meshVertexIds[1]=
          (sms.k+1)*(sms.j+1)*(i+ii) + (sms.k+1)*(j+1) + (k+kk);
    }
  }

    /*lastly the k edges k remains constant*/
  for(ii=0;ii<2;++ii){
    for(jj=0;jj<2;++jj){
      edgeId = ((sms.k)*(sms.j+1)*(i+ii)) + (sms.k*(j+jj)) +k + iedges + jedges;
      edgeContainer.edgeList[edgeId].alignment = 2;
      edgeContainer.edgeList[edgeId].i = i +ii;
      edgeContainer.edgeList[edgeId].j = j+jj;
      edgeContainer.edgeList[edgeId].k = k;

      edgeContainer.edgeList[edgeId].meshVertexIds[0]=
          (sms.k+1)*(sms.j+1)*(i+ii) + (sms.k+1)*(j+jj) +k;
      edgeContainer.edgeList[edgeId].meshVertexIds[1]=
          (sms.k+1)*(sms.j+1)*(i+ii) + (sms.k+1)*(j+jj) +(k+1);
    }
  } 
}
void createMeshCellVertices( int i, int j, int k)
{
  extern SuperMeshSize sms;
  int ii,jj,kk;
  
  
  int vertexId = 0;
  
  int nIVerts = sms.i+1;
  int nJVerts = sms.j+1;
  int nKVerts = sms.k+1; 
 
    /*lets create all the topology vertices for this block*/
    /*The vertices are numbered in k first, then increasing in j,
      then increasing in i */    
  
  for(ii=0;ii<2;++ii){
    for(jj=0;jj<2;++jj){
      for(kk=0;kk<2;++kk){
        vertexId = nKVerts*nJVerts*(ii+i)+
            nKVerts*(jj+j) + (kk+k);
        vertexContainer.vertexList[vertexId].i = i+ii;
        vertexContainer.vertexList[vertexId].j = j+jj;
        vertexContainer.vertexList[vertexId].k = k+kk;       
      }
    }
  }

}


int getMaxEdges( )
{
  extern SuperMeshSize sms;
  return ((sms.i)*(sms.j+1)*(sms.k+1))+
      ((sms.i+1)*sms.j*(sms.k+1))+
      ((sms.i+1)*(sms.j+1)*(sms.k));
}
int getMaxVertices( )
{
  extern SuperMeshSize sms;
  return (sms.i+1)*(sms.j+1)*(sms.k+1);
}
int getMaxFaces( )
{
  extern SuperMeshSize sms;
  return ((sms.i+1)*sms.j*sms.k)+
      (sms.i*(sms.j+1)*sms.k)+
      (sms.i*sms.j*(sms.k+1));
}
int getMaxCells( )
{
  extern SuperMeshSize sms;
  return sms.i*sms.j*sms.k;
}


int getMeshCellNodeSetSize( int cellId ,int getAll){
  extern NumZones numZones;

  int maxCells = getMaxCells( );
  if(cellId>=maxCells)
      return -1;
  MeshCell cell = cellContainer.cellList[cellId];
  if(getAll==1){
      /*We need to look at how many zones are unstructured and add that
        amount to the strucutred zones here.  A cell knows how many subdivisons
        it has (same as number of subdivided zones).  We'll multiply this by
        8 which is the number of new nodes for each subdivision*/
    
      return (numZones.iZones[cell.i] + 1) * (numZones.jZones[cell.j] + 1) *
          (numZones.kZones[cell.k] + 1) + (cell.numberSubdivisions*8);
  }
  
  else
      return cell.numNodes;
  
}

int getMeshCellFaceNodeSetSize( int faceId, int getAll ){

  extern NumZones numZones;
  
  int maxFaces = getMaxFaces( );
  if(faceId>=maxFaces)
      return -1;

  MeshFace face = faceContainer.faceList[faceId];
  if(getAll==1){
    if(face.alignment == 0){
        /*i normal face*/
      return (numZones.jZones[face.j] +1 ) * (numZones.kZones[face.k] + 1);
    }
    else if(face.alignment == 1){
        /*j normal face*/
      return (numZones.iZones[face.i] + 1)* (numZones.kZones[face.k] + 1);
    }
    else{
        /*k normal face*/
      return (numZones.iZones[face.i] +1) * (numZones.jZones[face.j] + 1);
    }
  }
  else
      return face.numNodes;
  
  
}
int getMeshCellEdgeNodeSetSize( int edgeId , int getAll){
    /*returns the nodes of this edge and its two associated vertices*/
  int maxEdges = getMaxEdges( );
  extern NumZones numZones;

  if (edgeId>=maxEdges)
      return -1;

  MeshEdge edge = edgeContainer.edgeList[edgeId];

  if(getAll == 1){
    if(edge.alignment == 0){
        /*i aligned edge*/
      return numZones.iZones[edge.i] + 1;
    }
    else if (edge.alignment == 1){
        /*j aligned edge*/
      return numZones.jZones[edge.j] + 1;
    }
    else{
      return numZones.kZones[edge.k] + 1;
    }
  }
  else
      return edge.numNodes;
  
  
}
#ifdef USE_MPI
int numDomainNodes( )
{
  extern int blockProcessorMap[CMG_MAX_BLOCK_DEFINITIONS];
  int myRank = 0;
  MPI_Comm_rank(MPI_COMM_WORLD, &myRank);
  MeshCell cell = cellContainer.cellList[blockProcessorMap[myRank]];
  MeshEdge edge;
  MeshFace face;
  MeshVertex vertex;
  int i,j,k;
  int cellId = blockProcessorMap[myRank];
  int counter = 0;
  
  
    /*vertices*/
  for(i=0;i<2;++i){
    for(j=0;j<2;++j){
      for(k=0;k<2;++k){
        int vertexId =getVertexId(cell.i+i,cell.j+j,cell.k+k);
        if(cellId == getMeshCellVertexOwner(vertexId))
            counter++;
      }
    }
  }
    /*edges*/
  for(j=0;j<2;++j){
    for(k=0;k<2;++k){
      int edgeId = getEdgeId(cell.i,cell.j+j,cell.k+k,0);
      if(cellId == getMeshCellEdgeOwner(edgeId)){
        edge = edgeContainer.edgeList[edgeId];
        counter+=edge.numNodes;
      }
    }
  }
  for(k=0;k<2;++k){
    for(i=0;i<2;++i){
      int edgeId = getEdgeId(cell.i + i,cell.j,cell.k+k,1);
      if(cellId == getMeshCellEdgeOwner(edgeId)){ 
        edge = edgeContainer.edgeList[edgeId];
        counter+=edge.numNodes;
      }
    }
  }
  for(i=0;i<2;++i){
    for(j=0;j<2;++j){
      int edgeId = getEdgeId(cell.i + i,cell.j + j,cell.k,2);
      if(cellId == getMeshCellEdgeOwner(edgeId)){
        edge = edgeContainer.edgeList[edgeId];
        counter+=edge.numNodes;
      }
    }
  }

    /*faces*/
  for(i=0;i<2;++i){
    int faceId = getFaceId(cell.i + i,cell.j,cell.k,0);
    if(cellId == getMeshCellFaceOwner(faceId)){
      face = faceContainer.faceList[faceId];
      counter+=face.numNodes;
    }
  }
  for(j=0;j<2;++j){
    int faceId = getFaceId(cell.i,cell.j + j,cell.k,1);
    if(cellId == getMeshCellFaceOwner(faceId)){
      face = faceContainer.faceList[faceId];
      counter+=face.numNodes;
    }
  }
  for(k=0;k<2;++k){
    int faceId = getFaceId(cell.i,cell.j,cell.k + k,2);
    if(cellId == getMeshCellFaceOwner(faceId)){
      face = faceContainer.faceList[faceId];
      counter+=face.numNodes;
    }
  }

  counter+=cell.numNodes;

  return counter;
}

void getDomainNodes(int *nodeList)
{
  extern int blockProcessorMap[CMG_MAX_BLOCK_DEFINITIONS];
  int myRank = 0;
  MPI_Comm_rank(MPI_COMM_WORLD, &myRank);
  MeshCell cell = cellContainer.cellList[blockProcessorMap[myRank]];
  MeshEdge edge;
  MeshFace face;
  MeshVertex vertex;
  int i,j,k,z;
  int cellId = blockProcessorMap[myRank];
  int counter = 0;
  
  
    /*vertices*/
  for(i=0;i<2;++i){
    for(j=0;j<2;++j){
      for(k=0;k<2;++k){
        int vertexId =getVertexId(cell.i+i,cell.j+j,cell.k+k);
        if(cellId == getMeshCellVertexOwner(vertexId)){
          nodeList[counter] = getMeshCellVertexNode(vertexId);
          counter++;
        }
      }
    }
  }
    /*edges*/
  for(j=0;j<2;++j){
    for(k=0;k<2;++k){
      int edgeId = getEdgeId(cell.i,cell.j+j,cell.k+k,0);
      if(cellId == getMeshCellEdgeOwner(edgeId)){
        edge = edgeContainer.edgeList[edgeId];
        for(z=0;z<edge.numNodes;++z){
          nodeList[counter] = edge.baseNodeId+z;
          counter++;
        }
      }
    }
  }
  for(k=0;k<2;++k){
    for(i=0;i<2;++i){
      int edgeId = getEdgeId(cell.i + i,cell.j,cell.k+k,1);
      if(cellId == getMeshCellEdgeOwner(edgeId)){ 
        edge = edgeContainer.edgeList[edgeId];
        for(z=0;z<edge.numNodes;++z){
          nodeList[counter] = edge.baseNodeId+z;
          counter++;
        }
      }
    }
  }
  for(i=0;i<2;++i){
    for(j=0;j<2;++j){
      int edgeId = getEdgeId(cell.i + i,cell.j + j,cell.k,2);
      if(cellId == getMeshCellEdgeOwner(edgeId)){
        edge = edgeContainer.edgeList[edgeId];
        for(z=0;z<edge.numNodes;++z){
          nodeList[counter] = edge.baseNodeId+z;
          counter++;
        }
      }
    }
  }

    /*faces*/
  for(i=0;i<2;++i){
    int faceId = getFaceId(cell.i + i,cell.j,cell.k,0);
    if(cellId == getMeshCellFaceOwner(faceId)){
      face = faceContainer.faceList[faceId];
      for(z=0;z<face.numNodes;++z){
        nodeList[counter] = face.baseNodeId+z;
        counter++;
      }
    }
  }
  for(j=0;j<2;++j){
    int faceId = getFaceId(cell.i,cell.j + j,cell.k,1);
    if(cellId == getMeshCellFaceOwner(faceId)){
      face = faceContainer.faceList[faceId];
      for(z=0;z<face.numNodes;++z){
        nodeList[counter] = face.baseNodeId+z;
        counter++;
      }
    }
  }
  for(k=0;k<2;++k){
    int faceId = getFaceId(cell.i,cell.j,cell.k + k,2);
    if(cellId == getMeshCellFaceOwner(faceId)){
      face = faceContainer.faceList[faceId];
      for(z=0;z<face.numNodes;++z){
        nodeList[counter] = face.baseNodeId+z;
        counter++;
      }
    }
  }

  for(z =0;z<cell.numNodes;++z){
    nodeList[counter++] = cell.baseNodeId+z;
  } 

}


#endif


void getMeshCellNodeSet( int cellId, int *nodeList , int getAll){
  MeshCell cell = cellContainer.cellList[cellId];
  MeshEdge edge;
  MeshFace face;
  MeshVertex vertex;
  int i,j,k;
  int counter = 0;
  
  if(getAll==1){
      /*vertices*/
    for(i=0;i<2;++i){
      for(j=0;j<2;++j){
        for(k=0;k<2;++k){
          vertex = vertexContainer.vertexList[getVertexId(cell.i+i,
                                                          cell.j+j,
                                                          cell.k+k)];
          nodeList[((i*1)+(j*2)+(k*4))] = vertex.nodeId;
        }
      }
    }
    counter +=8;

      /*edges*/
    for(j=0;j<2;++j){
      for(k=0;k<2;++k){
        edge = edgeContainer.edgeList[getEdgeId(cell.i,
                                                cell.j+j,
                                                cell.k+k,0)];
        for(i = 0;i<edge.numNodes;++i)
            nodeList[counter+i] = edge.baseNodeId+i;
        counter += edge.numNodes;
      }
    }
    for(k=0;k<2;++k){
      for(i=0;i<2;++i){
        edge = edgeContainer.edgeList[getEdgeId(cell.i + i,
                                                cell.j,
                                                cell.k+k,1)];
        for(j = 0;j<edge.numNodes;++j)
            nodeList[counter+j] = edge.baseNodeId+j;
        counter += edge.numNodes;
      }
    }
    for(i=0;i<2;++i){
      for(j=0;j<2;++j){
        edge = edgeContainer.edgeList[getEdgeId(cell.i + i,
                                                cell.j + j,
                                                cell.k,2)];
        for(k = 0;k<edge.numNodes;++k)
            nodeList[counter+k] = edge.baseNodeId+k;
        counter += edge.numNodes; 
      }
    }

      /*faces*/
    for(i=0;i<2;++i){
      face = faceContainer.faceList[getFaceId(cell.i + i,
                                              cell.j,cell.k,0)];
      for(j = 0;j<face.numNodes;++j)
          nodeList[counter + j] = face.baseNodeId + j;
      counter += face.numNodes;
    }
    for(j=0;j<2;++j){
      face = faceContainer.faceList[getFaceId(cell.i,cell.j + j,
                                              cell.k,1)];
      for(k = 0;k<face.numNodes;++k)
          nodeList[counter+k] = face.baseNodeId+k;
      counter += face.numNodes;
    }
    for(k=0;k<2;++k){
      face = faceContainer.faceList[getFaceId(cell.i,cell.j,
                                              cell.k + k,2)];
      for(i = 0;i<face.numNodes;++i)
          nodeList[counter + i] = face.baseNodeId + i;
      counter += face.numNodes;
    }

      /*finally the cell*/
    for(i = 0;i<cell.numNodes;++i)
        nodeList[counter + i] = cell.baseNodeId + i;
    
    
  }
  else{
    for(i = 0;i<cell.numNodes;++i)
        nodeList[i] = cell.baseNodeId + i;
  }

}
void getMeshCellFaceNodeSet( int faceId, int *nodeList ,int getAll){
  MeshFace face = faceContainer.faceList[faceId];
  MeshEdge edge0,edge1,edge2,edge3;
  MeshVertex vertex0,vertex1,vertex2,vertex3;
  int i;
  

  if(getAll==1){
    int counter = 0;
    
    vertex0 = vertexContainer.vertexList[getVertexId(face.i,face.j,face.k)];
    if(face.alignment==0){
        /*i normal face*/
      vertex1 = vertexContainer.vertexList[getVertexId(face.i,face.j+1,face.k)];
      vertex2 = vertexContainer.vertexList[getVertexId(face.i,face.j+1,face.k+1)];
      vertex3 = vertexContainer.vertexList[getVertexId(face.i,face.j,face.k+1)];
      edge0 = edgeContainer.edgeList[getEdgeId(face.i,face.j,face.k,1)];
      edge1 = edgeContainer.edgeList[getEdgeId(face.i,face.j+1,face.k,2)];
      edge2 = edgeContainer.edgeList[getEdgeId(face.i,face.j,face.k+1,1)];
      edge3 = edgeContainer.edgeList[getEdgeId(face.i,face.j,face.k,2)]; 
    }
    else if(face.alignment==1){
        /*j normal face*/
      vertex1 = vertexContainer.vertexList[getVertexId(face.i+1,face.j,face.k)];
      vertex2 = vertexContainer.vertexList[getVertexId(face.i+1,face.j,face.k+1)];
      vertex3 = vertexContainer.vertexList[getVertexId(face.i,face.j,face.k+1)];
      edge0 = edgeContainer.edgeList[getEdgeId(face.i,face.j,face.k,0)];
      edge1 = edgeContainer.edgeList[getEdgeId(face.i+1,face.j,face.k,2)];
      edge2 = edgeContainer.edgeList[getEdgeId(face.i,face.j,face.k+1,0)];
      edge3 = edgeContainer.edgeList[getEdgeId(face.i,face.j,face.k,2)]; 
    }
    else{
        /*k normal face*/
      vertex1 = vertexContainer.vertexList[getVertexId(face.i+1,face.j,face.k)];
      vertex2 = vertexContainer.vertexList[getVertexId(face.i+1,face.j+1,face.k)];
      vertex3 = vertexContainer.vertexList[getVertexId(face.i,face.j+1,face.k)];
      edge0 = edgeContainer.edgeList[getEdgeId(face.i,face.j,face.k,0)];
      edge1 = edgeContainer.edgeList[getEdgeId(face.i+1,face.j,face.k,1)];
      edge2 = edgeContainer.edgeList[getEdgeId(face.i,face.j+1,face.k,0)];
      edge3 = edgeContainer.edgeList[getEdgeId(face.i,face.j,face.k,1)]; 
      
    }

    nodeList[0] = vertex0.nodeId;
    nodeList[1] = vertex1.nodeId;
    nodeList[2] = vertex2.nodeId;
    nodeList[3] = vertex3.nodeId;

    counter += 4;

    for(i=0;i<edge0.numNodes;++i)
        nodeList[i+counter] = edge0.baseNodeId + i;
    counter += edge0.numNodes;

    for(i=0;i<edge1.numNodes;++i)
        nodeList[i+counter] = edge1.baseNodeId + i;
    counter += edge1.numNodes;

    for(i=0;i<edge2.numNodes;++i)
        nodeList[i+counter] = edge2.baseNodeId + i;
    counter += edge2.numNodes;

    for(i=0;i<edge3.numNodes;++i)
        nodeList[i+counter] = edge3.baseNodeId + i;
    counter += edge3.numNodes;

    for(i=0;i<face.numNodes;++i)
        nodeList[i+counter] = face.baseNodeId+i;
  }
  else{
    for(i=0;i<face.numNodes;++i)
        nodeList[i] = face.baseNodeId+i;
  }
}

void getMeshCellEdgeNodeSet( int edgeId, int *nodeList ,int getAll){
  MeshEdge edge = edgeContainer.edgeList[edgeId];
  MeshVertex vertex1,vertex2;
  int i;
  
  if(getAll==1){
    vertex1 = vertexContainer.vertexList[getVertexId(edge.i,edge.j,edge.k)];
    if(edge.alignment==0)
        vertex2 = vertexContainer.vertexList[getVertexId(edge.i+1,edge.j,edge.k)];
    else if(edge.alignment ==1)
        vertex2 = vertexContainer.vertexList[getVertexId(edge.i,edge.j+1,edge.k)];
    else
        vertex2 = vertexContainer.vertexList[getVertexId(edge.i,edge.j,edge.k+1)];

    nodeList[0] = vertex1.nodeId;
    nodeList[1] = vertex2.nodeId;
    
    for(i=0;i<edge.numNodes;++i)
        nodeList[i+2] = edge.baseNodeId+i;
    
  }
  else{
    for(i=0;i<edge.numNodes;++i)
        nodeList[i] = edge.baseNodeId+i;
  } 
}

int getMeshCellVertexNode( int vertexId ){
  int maxVertices = getMaxVertices( );
  if(vertexId>=maxVertices)
      return -1;
  
  return vertexContainer.vertexList[vertexId].nodeId;
}
    
int getVertexId(int i, int j, int k){
  extern SuperMeshSize sms;
  int vertexId= ((sms.j+1)*(sms.k+1)*i) + ((sms.k+1)*j) + k;
  return vertexId;
  
  

}
int getEdgeId( int i, int j, int k, int alignment){
  extern SuperMeshSize sms;
  int iedges = sms.i * (sms.j+1) *(sms.k+1);
  int jedges = (sms.i+1) *sms.j*(sms.k+1);

  if(alignment ==0)
      return ((sms.k+1)*(sms.j+1)*i) + ((sms.k+1)*j) + k;
  else if(alignment ==1)
      return ((sms.k+1)*sms.j*i) + ((sms.k+1)*j) + k + iedges;
  else
      return ((sms.k)*(sms.j+1)*i) + (sms.k*j) + k + iedges + jedges;
  
  
}
int getFaceId( int i, int j, int k, int alignment){
  extern SuperMeshSize sms;
  
  int inormalfaces = (sms.i+1) *sms.j*sms.k;
  int jnormalfaces = (sms.j+1) *sms.i*sms.k;

  if(alignment==0)
      return (sms.k*sms.j*i) + (sms.k*j) + k;
  else if(alignment == 1)
      return (sms.k*(sms.j+1)*i) + (sms.k*j) + k + inormalfaces;
  else
      return ((sms.k+1)*sms.j*i) + ((sms.k+1)*j) + k + inormalfaces + jnormalfaces;
  

}
int getCellId(int i, int j, int k){
  extern SuperMeshSize sms;

  if(i <0 ||
     j<0 ||
     k<0)
      return -1;

  if(i>sms.i-1 ||
     j>sms.j-1 ||
     k>sms.k-1)
      return -1;
  
  

  return (sms.k*sms.j*i) + (sms.k*j) + k;
  
}

int getVertices(Range iRange, Range jRange, Range kRange,
                int **vertexList)
{
  int i,j,k;
  int size=0;
  int counter = 0;
  int domainId = -1;
  

#ifdef USE_MPI
  MPI_Comm_rank(MPI_COMM_WORLD, &domainId);
  extern int blockProcessorMap[CMG_MAX_BLOCK_DEFINITIONS];
#endif
  
    /*figure out size first*/
  for(i= iRange.min;i<=iRange.max;++i){
    for(j=jRange.min;j<=jRange.max;++j){
      for(k = kRange.min;k<=kRange.max;++k){
#ifdef USE_MPI
        if(getMeshCellVertexOwner(getVertexId(i,j,k)) ==
           blockProcessorMap[domainId])
            size +=1;
#else
        size +=1;
#endif
      }
    }
  }

  *vertexList = malloc(sizeof(int) * size);

  for(i= iRange.min;i<=iRange.max;++i){
    for(j=jRange.min;j<=jRange.max;++j){
      for(k = kRange.min;k<=kRange.max;++k){
#ifdef USE_MPI
        if(getMeshCellVertexOwner(getVertexId(i,j,k)) ==
           blockProcessorMap[domainId]){
          (*vertexList)[counter] = getVertexId(i,j,k);
          ++counter;
        }
#else
        (*vertexList)[counter] = getVertexId(i,j,k);
        ++counter;
#endif
      }                                     
    }
  }

  return size;
}

int getEdges(Range iRange, Range jRange, Range kRange,
             int **edgeList)
{
    /*Note if this is an edgeTag list range, edge tags are not supported
      yet. This range will get edges for a nodeTag range.
    */
  int i,j,k;
  int size=0;
  int counter = 0;

  int domainId = -1;
  

#ifdef USE_MPI
  MPI_Comm_rank(MPI_COMM_WORLD, &domainId);
  extern int blockProcessorMap[CMG_MAX_BLOCK_DEFINITIONS];
#endif

    /*i edges first*/
  for(i= iRange.min;i<iRange.max;++i){
    for(j=jRange.min;j<=jRange.max;++j){
      for(k = kRange.min;k<=kRange.max;++k){
#ifdef USE_MPI
        if(getMeshCellEdgeOwner(getEdgeId(i,j,k,0)) ==
           blockProcessorMap[domainId])
            size +=1;
#else
        size +=1;
#endif
      }
    }
  }
  for(i= iRange.min;i<=iRange.max;++i){
    for(j=jRange.min;j<jRange.max;++j){
      for(k = kRange.min;k<=kRange.max;++k){
#ifdef USE_MPI
        if(getMeshCellEdgeOwner(getEdgeId(i,j,k,1)) ==
           blockProcessorMap[domainId])
            size +=1;
#else
        size +=1;
#endif 
      }
    }
  }
  for(i= iRange.min;i<=iRange.max;++i){
    for(j=jRange.min;j<=jRange.max;++j){
      for(k = kRange.min;k<kRange.max;++k){
#ifdef USE_MPI
        if(getMeshCellEdgeOwner(getEdgeId(i,j,k,2)) ==
           blockProcessorMap[domainId])
            size +=1;
#else
        size +=1;
#endif
      }
    }
  }

  *edgeList = malloc(sizeof(int) * size);

  for(i= iRange.min;i<iRange.max;++i){
    for(j=jRange.min;j<=jRange.max;++j){
      for(k = kRange.min;k<=kRange.max;++k){
#ifdef USE_MPI
        if(getMeshCellEdgeOwner(getEdgeId(i,j,k,0)) ==
           blockProcessorMap[domainId]){  
          (*edgeList)[counter] = getEdgeId(i,j,k,0);
          counter++;
        }
#else
        (*edgeList)[counter] = getEdgeId(i,j,k,0);
        counter++;
#endif
      }
    }
  }
  for(i= iRange.min;i<=iRange.max;++i){
    for(j=jRange.min;j<jRange.max;++j){
      for(k = kRange.min;k<=kRange.max;++k){
#ifdef USE_MPI
        if(getMeshCellEdgeOwner(getEdgeId(i,j,k,1)) ==
           blockProcessorMap[domainId]){  
          (*edgeList)[counter] = getEdgeId(i,j,k,1);
          counter++;
        }
#else
        (*edgeList)[counter] = getEdgeId(i,j,k,1);
        counter++;
#endif
      }
    }
  }
  for(i= iRange.min;i<=iRange.max;++i){
    for(j=jRange.min;j<=jRange.max;++j){
      for(k = kRange.min;k<kRange.max;++k){
#ifdef USE_MPI
        if(getMeshCellEdgeOwner(getEdgeId(i,j,k,2)) ==
           blockProcessorMap[domainId]){  
          (*edgeList)[counter] = getEdgeId(i,j,k,2);
          counter++;
        }
#else
        (*edgeList)[counter] = getEdgeId(i,j,k,2);
        counter++;
#endif 
      }
    }
  }
  return size;
  

}

int getFaces(Range iRange, Range jRange, Range kRange,
             int **faceList)
{
    /*this will work for face tags as well as node tags*/

  int i,j,k;
  int size=0;
  int counter = 0;
  int domainId = -1;
  
#ifdef USE_MPI
  MPI_Comm_rank(MPI_COMM_WORLD, &domainId);
  extern int blockProcessorMap[CMG_MAX_BLOCK_DEFINITIONS];
#endif
  
    /*i faces first*/
  for(i= iRange.min;i<=iRange.max;++i){
    for(j=jRange.min;j<jRange.max;++j){
      for(k = kRange.min;k<kRange.max;++k){
#ifdef USE_MPI
        if(getMeshCellFaceOwner(getFaceId(i,j,k,0)) ==
           blockProcessorMap[domainId])
            size +=1;
#else
        size +=1;
#endif
      }
    }
  }
  for(i= iRange.min;i<iRange.max;++i){
    for(j=jRange.min;j<=jRange.max;++j){
      for(k = kRange.min;k<kRange.max;++k){
#ifdef USE_MPI
        if(getMeshCellFaceOwner(getFaceId(i,j,k,1)) ==
           blockProcessorMap[domainId])
            size +=1;
#else
        size +=1;
#endif  
      }
    }
  }
  for(i= iRange.min;i<iRange.max;++i){
    for(j=jRange.min;j<jRange.max;++j){
      for(k = kRange.min;k<=kRange.max;++k){
#ifdef USE_MPI
        if(getMeshCellFaceOwner(getFaceId(i,j,k,2)) ==
           blockProcessorMap[domainId])
            size +=1;
#else
        size +=1;
#endif   
      }
    }
  }

  *faceList = malloc(sizeof(int) * size);

  for(i= iRange.min;i<=iRange.max;++i){
    for(j=jRange.min;j<jRange.max;++j){
      for(k = kRange.min;k<kRange.max;++k){
#ifdef USE_MPI
        if(getMeshCellFaceOwner(getFaceId(i,j,k,0)) ==
           blockProcessorMap[domainId]){
          (*faceList)[counter] = getFaceId(i,j,k,0);
          counter++;
        }
#else
        (*faceList)[counter] = getFaceId(i,j,k,0);
        counter++;
#endif
      }
    }
  }
  
  for(i= iRange.min;i<iRange.max;++i){
    for(j=jRange.min;j<=jRange.max;++j){
      for(k = kRange.min;k<kRange.max;++k){
#ifdef USE_MPI
        if(getMeshCellFaceOwner(getFaceId(i,j,k,1)) ==
           blockProcessorMap[domainId]){
          (*faceList)[counter] = getFaceId(i,j,k,1);
          counter++;
        }
#else
        (*faceList)[counter] = getFaceId(i,j,k,1);
        counter++;
#endif
      }
    }
  }
  
  for(i= iRange.min;i<iRange.max;++i){
    for(j=jRange.min;j<jRange.max;++j){
      for(k = kRange.min;k<=kRange.max;++k){
#ifdef USE_MPI
        if(getMeshCellFaceOwner(getFaceId(i,j,k,2)) ==
           blockProcessorMap[domainId]){
          (*faceList)[counter] = getFaceId(i,j,k,2);
          counter++;
        }
#else
        (*faceList)[counter] = getFaceId(i,j,k,2);
        counter++;
#endif
      }
    }
  }
  return size;
  
 
  
}

int getCells(Range iRange, Range jRange, Range kRange,
             int **cellList)
{

    /*All tags are passed in vertex space, this makes things less confusing
      and easier to code as well
    */
  int i,j,k;
  int size = 0;
  int counter = 0;
  int domainId = -1;
  
#ifdef USE_MPI
  MPI_Comm_rank(MPI_COMM_WORLD, &domainId);
  extern int blockProcessorMap[CMG_MAX_BLOCK_DEFINITIONS];
#endif

  for(i=iRange.min;i<iRange.max;++i){
    for(j=jRange.min;j<jRange.max;++j){
      for(k = kRange.min;k<kRange.max;++k){
#ifdef USE_MPI
        if(getCellId(i,j,k) ==
           blockProcessorMap[domainId])  
            size +=1;
#else
        size+=1;
#endif
      }
    }
  }

  *cellList = malloc(sizeof(int) * size);

  for(i= iRange.min;i<iRange.max;++i){
    for(j=jRange.min;j<jRange.max;++j){
      for(k = kRange.min;k<kRange.max;++k){
#ifdef USE_MPI
        if(getCellId(i,j,k) ==
           blockProcessorMap[domainId]){
          (*cellList)[counter] = getCellId(i,j,k);
          ++counter;
        }
#else
        (*cellList)[counter] = getCellId(i,j,k);
        ++counter;
#endif   
      }                                     
    }
  }

  return size;
  
}
void getMeshFaceFaces(MeshFace face, int *faces)
{
  int counter = 0;
  for(counter = 0;counter<face.numFaces;++counter){
    faces[counter] = face.baseFaceId + counter;
  }
}
void getCellNeighbors(MeshCell cell, int *faceNeighbors,
                      int *edgeNeighbors, int *vertexNeighbors)
{

  int i = cell.i;
  int j = cell.j;
  int k = cell.k;

    /*face neighbors, -i face, +i face,
      -j face, +j face, -k face, +k face */

  faceNeighbors[0] = getCellId(i-1,j,k);
  faceNeighbors[1] = getCellId(i+1,j,k);

  faceNeighbors[2] = getCellId(i,j-1,k);
  faceNeighbors[3] = getCellId(i,j+1,k);

  faceNeighbors[4] = getCellId(i,j,k-1);
  faceNeighbors[5] = getCellId(i,j,k+1);

    /*vertex Neighbors*/
  vertexNeighbors[0] = getCellId(i-1,j-1,k-1);
  vertexNeighbors[1] = getCellId(i-1,j-1,k+1);
  vertexNeighbors[2] = getCellId(i-1,j+1,k-1);
  vertexNeighbors[3] = getCellId(i-1,j+1,k+1);

  vertexNeighbors[4] = getCellId(i+1,j-1,k-1);
  vertexNeighbors[5] = getCellId(i+1,j-1,k+1);
  vertexNeighbors[6] = getCellId(i+1,j+1,k-1);
  vertexNeighbors[7] = getCellId(i+1,j+1,k+1);

    /*edgeNeighbors*/

  edgeNeighbors[0] = getCellId(i,j+1,k+1);
  edgeNeighbors[1] = getCellId(i,j-1,k+1);
  edgeNeighbors[2] = getCellId(i,j-1,k-1);
  edgeNeighbors[3] = getCellId(i,j+1,k-1);

  edgeNeighbors[4] = getCellId(i-1,j,k+1);
  edgeNeighbors[5] = getCellId(i-1,j,k-1);
  edgeNeighbors[6] = getCellId(i+1,j,k+1);
  edgeNeighbors[7] = getCellId(i+1,j,k-1);
  
  edgeNeighbors[8] = getCellId(i-1,j+1,k);
  edgeNeighbors[9] = getCellId(i-1,j-1,k);
  edgeNeighbors[10] = getCellId(i+1,j+1,k);
  edgeNeighbors[11] = getCellId(i+1,j-1,k);  
}
void getSendReceiveTopologyEntitiesSize(int *facesSize, int *edgesSize, int *verticesSize,
                                        int domainId, int send)
{

    /*for now get the cell on the given domain and and query all its neighbors*/

  
  
  MeshCell c = cellContainer.cellList[domainId];
  int cellFaces[6];
  int cellEdges[12];
  int cellVertices[8];

  int ii,jj,kk,alignment;
  for(ii =0;ii<2;++ii){
    for(jj=0;jj<2;++jj){
      for(kk=0;kk<2;++kk){
        cellVertices[(2*2*ii) +(2*jj) + kk] = getVertexId(c.i+ii,c.j+jj,c.k+kk);
      }
    }
  }

  for(alignment=0;alignment<3;++alignment){
    for(ii=0;ii<2;++ii){
      for(jj=0;jj<2;++jj){
        if(alignment==0)
            cellEdges[(4*alignment) + (2*ii) + jj] = getEdgeId(c.i,c.j+ii,c.k+jj,alignment);
        else if(alignment==1)
            cellEdges[(4*alignment) + (2*ii) + jj] = getEdgeId(c.i+ii,c.j,c.k+jj,alignment);
        else if(alignment==2)
            cellEdges[(4*alignment) + (2*ii) + jj] = getEdgeId(c.i+ii,c.j+jj,c.k,alignment);
      }
    }
  }
  for(alignment=0;alignment<3;++alignment){
    for(ii=0;ii<2;++ii){
      if(alignment ==0)
          cellFaces[(2*alignment) +ii] = getFaceId(c.i+ii,c.j,c.k,alignment);
      else if(alignment ==1)
          cellFaces[(2*alignment) +ii] = getFaceId(c.i,c.j+ii,c.k,alignment);
      else if(alignment ==2)
          cellFaces[(2*alignment) +ii] = getFaceId(c.i,c.j,c.k+ii,alignment);
    }
  }

  (*facesSize)=0;
  (*edgesSize)=0;
  (*verticesSize) = 0;
  
  
    
  
  for(ii=0;ii<6;++ii){
    if(send==0 && getMeshCellFaceOwner(cellFaces[ii])<domainId)
        (*facesSize)+=1;
    if(send==1 && getMeshCellFaceOwner(cellFaces[ii]) == domainId)
        (*facesSize)+=1;
  }
  for(ii=0;ii<12;++ii){
    if(send==0 && getMeshCellEdgeOwner(cellEdges[ii])<domainId)
        (*edgesSize)+=1;
    if(send==1 && getMeshCellEdgeOwner(cellEdges[ii]) == domainId)
        (*edgesSize)+=1;
  }
  for(ii=0;ii<8;++ii){
    if(send==0 && getMeshCellVertexOwner(cellVertices[ii])<domainId)
        (*verticesSize)+=1;
    if(send==1 && getMeshCellVertexOwner(cellVertices[ii]) == domainId)
        (*verticesSize)+=1;
  }

  
}

void getSendReceiveTopologyEntities( int *faces, int *edges, int *vertices, int domainId,
                                     int send)
{
  MeshCell c = cellContainer.cellList[domainId];
  int cellFaces[6];
  int cellEdges[12];
  int cellVertices[8];

  int ii,jj,kk,alignment;
  for(ii =0;ii<2;++ii){
    for(jj=0;jj<2;++jj){
      for(kk=0;kk<2;++kk){
        cellVertices[(2*2*ii) +(2*jj) + kk] = getVertexId(c.i+ii,c.j+jj,c.k+kk);
      }
    }
  }

  for(alignment=0;alignment<3;++alignment){
    for(ii=0;ii<2;++ii){
      for(jj=0;jj<2;++jj){
        if(alignment==0)
            cellEdges[(4*alignment) + (2*ii) + jj] = getEdgeId(c.i,c.j+ii,c.k+jj,alignment);
        else if(alignment==1)
            cellEdges[(4*alignment) + (2*ii) + jj] = getEdgeId(c.i+ii,c.j,c.k+jj,alignment);
        else if(alignment==2)
            cellEdges[(4*alignment) + (2*ii) + jj] = getEdgeId(c.i+ii,c.j+jj,c.k,alignment);
      }
    }
  }
  for(alignment=0;alignment<3;++alignment){
    for(ii=0;ii<2;++ii){
      if(alignment ==0)
          cellFaces[(2*alignment) +ii] = getFaceId(c.i+ii,c.j,c.k,alignment);
      else if(alignment ==1)
          cellFaces[(2*alignment) +ii] = getFaceId(c.i,c.j+ii,c.k,alignment);
      else if(alignment ==2)
          cellFaces[(2*alignment) +ii] = getFaceId(c.i,c.j,c.k+ii,alignment);
    }
  }

  int counter = 0;
  
  
  

  for(ii=0;ii<6;++ii){
    if(send==0 && getMeshCellFaceOwner(cellFaces[ii])<domainId)
        faces[counter++] = cellFaces[ii];
    if(send==1 && getMeshCellFaceOwner(cellFaces[ii]) == domainId)
        faces[counter++] = cellFaces[ii];
  }
  counter = 0;
  
  for(ii=0;ii<12;++ii){
    if(send==0 && getMeshCellEdgeOwner(cellEdges[ii])<domainId)
        edges[counter++] = cellEdges[ii];
    if(send==1 && getMeshCellEdgeOwner(cellEdges[ii]) == domainId)
        edges[counter++] = cellEdges[ii];

  }
  
  counter = 0;

  for(ii=0;ii<8;++ii){
    if(send==0 && getMeshCellVertexOwner(cellVertices[ii])<domainId)
        vertices[counter++] = cellVertices[ii];
    if(send==1 && getMeshCellVertexOwner(cellVertices[ii]) == domainId)
        vertices[counter++] = cellVertices[ii];

  }

}

int getMeshCellVertexOwner(int vertexId)
{
  int cellIds[8];
  getMeshCellsFromVertex(cellIds,vertexId);

  int cellId = -1;
  int ii;
  for(ii=0;ii<8;++ii){
    if(cellId == -1 &&
       cellIds[ii] >-1)
        cellId = cellIds[ii];
    else if(cellIds[ii]<cellId &&
            cellIds[ii] > -1)
        cellId = cellIds[ii];
  }
  return cellId;
  
}

int getMeshCellEdgeOwner(int edgeId)
{
  int cellIds[4];
  getMeshCellsFromEdge(cellIds,edgeId);

  int cellId = -1;
  int ii;
  for(ii=0;ii<4;++ii){
    if(cellId == -1 &&
       cellIds[ii] >-1)
        cellId = cellIds[ii];
    else if(cellIds[ii]<cellId &&
            cellIds[ii]>-1)
        cellId = cellIds[ii];
  }
  return cellId;
      
              
}

int getMeshCellFaceOwner(int faceId)
{
  int cellIds[2];
  getMeshCellsFromFace(cellIds,faceId);

  int cellId = -1;
  int ii;
  for(ii=0;ii<2;++ii){
    if(cellId == -1 &&
       cellIds[ii] >-1)
        cellId = cellIds[ii];
    else if(cellIds[ii]<cellId &&
            cellIds[ii] > -1)
        cellId = cellIds[ii];
  }
  return cellId;
  
}

void getMeshCellsFromVertex(int *cellIds, int vertexId)
{
  extern SuperMeshSize sms;
  
  MeshVertex v  = vertexContainer.vertexList[vertexId];

  int i = v.i;
  int j = v.j;
  int k = v.k;
  int counter = 0;

  int ii,jj,kk;

  for(ii=0;ii<8;++ii)
      cellIds[ii] = -1;
  
  for(ii = -1;ii<1;++ii){
    for(jj = -1;jj<1;++jj){
      for(kk = -1;kk<1;++kk){
        if(i+ii == -1 ||
           j+jj == -1 ||
           k+kk == -1)
            continue;
        if(i+ii > sms.i +1 ||
           j+jj > sms.j +1 ||
           k+kk > sms.k +1 )
            continue;
        
        int temp = getCellId(i+ii,j+jj,k+kk);
        if(temp>-1)
            cellIds[counter++] = temp;
        
      }
    }
  }
 
}
void getMeshCellsFromEdge(int *cellIds, int edgeId)
{
  MeshEdge e = edgeContainer.edgeList[edgeId];
  extern SuperMeshSize sms;
  
    /*we have three different alignments, so will have to
      be careful about what counters we increment through*/

  int counter, counter1,temp;
  int counter2 = 0;
  
  
  for(counter = 0;counter<4;++counter)
      cellIds[counter] = -1;
  
  for(counter = -1;counter<1;++counter){
    for(counter1 = -1;counter1<1;++counter1){
      if(e.alignment ==0){
          /* i aligned edge, so cycle
             through k and j*/
        if(e.j+counter == -1 ||
           e.k+counter1 == -1 )
            continue;
        if(e.j+counter > sms.j+1 ||
           e.k+counter1 >sms.k+1)
            continue;
        temp = getCellId(e.i,e.j+counter, e.k+counter1);
        if(temp>-1)
            cellIds[counter2++]= temp; 
        
      }
      else if(e.alignment == 1){
          /* j aligned edge, so cycle
             through k and i*/
        if(e.i+counter == -1 ||
           e.k+counter1 == -1 )
            continue;
        if(e.i+counter > sms.i+1 ||
           e.k+counter1 >sms.k+1)
            continue;
        temp = getCellId(e.i+counter,e.j, e.k+counter1);
        if(temp>-1)
            cellIds[counter2++]=temp;
      }
      else if(e.alignment == 2){
          /* k aligned edge, so cycle
             through j and i*/
        if(e.i+counter == -1 ||
           e.j+counter1 == -1 )
            continue;
        if(e.i+counter > sms.i +1||
           e.j+counter1 >sms.j+1)
            continue;
        temp = getCellId(e.i+counter,e.j+counter1, e.k);
        if(temp>-1)
            cellIds[counter2++]=temp;
      }
    }
  }    
}
void getMeshCellsFromFace(int *cellIds, int faceId)
{
  MeshFace f = faceContainer.faceList[faceId];
  extern SuperMeshSize sms;
  
  int counter,temp,counter1;
  for(counter=0;counter<2;++counter)
      cellIds[counter] = -1;

  counter1=0;
  

  for(counter = -1;counter<1;++counter){

    if(f.alignment ==0){
        /*i normal face*/
      if(f.i +counter == -1||
         f.i +counter > sms.i +1)
          continue;
      temp = getCellId(f.i+counter,f.j,f.k);
      
      if(temp>-1)
          cellIds[counter1++] = temp;
      
    }
    else if(f.alignment == 1){
        /*j normal face*/
      if(f.j + counter == -1 ||
         f.j + counter > sms.j + 1)
          continue;
      temp = getCellId(f.i,f.j+counter,f.k);
      if(temp>-1)
          cellIds[counter1++] = temp;
    }
    else if(f.alignment == 2){
        /*k normal face*/
      if(f.k + counter == -1 ||
         f.k + counter > sms.k + 1)
          continue;
      temp = getCellId(f.i,f.j,f.k+counter);
      if(temp>-1)
          cellIds[counter1++] = temp;
    }
    
    
  }
}



                 
