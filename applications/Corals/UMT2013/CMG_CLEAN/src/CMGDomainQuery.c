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

  This file contains the implementation of the methods from cmg.h which
  will be used to query specific domain related data, ie send and recieve
  lists etc.

*/
#include <assert.h>
#include <stdlib.h>
#include "meshAndInputData.h"
#include "dataTypes.h"
#include "cmg.h"
#include "CMGDebug.h"
#include "CMGMeshTopology.h"
#include "CMGDomainQuery.h"
#ifdef USE_MPI
#include "mpi.h"
#endif

extern int blockProcessorMap[CMG_MAX_BLOCK_DEFINITIONS];

/*!
  Get the size of the local to global faces array
*/
void cltgfsz ( int *size ){ (*size) = 0;assert( "Not implemented" ); }
/*! Fortran cltgfsz() */
void cltgfsz_( int *size ){ 
  cltgfsz( size );
}

/*!
  Get the local to global faces array
*/
void cltgf ( int *faces ){ (*faces)=0;assert( "Not implemented" ); }
/*! Fortran cltgf() */
void cltgf_( int *faces ){ 
  cltgf( faces );
}

/*!
  Get the size of the local to global nodes array
*/
void cltgnsz ( int *size ){ (*size)=0;assert( "Not implemented" ); }
/*! Fortran cltgnsz() */
void cltgnsz_( int *size ){ 
  cltgnsz( size );
}

/*!
  Get the local to global nodes array
*/
void cltgn (int *nodes ){ (*nodes)=0;assert( "Not implemented" ); }
/*! Fortran cltgn() */
void cltgn_(int *nodes ){ 
  cltgn( nodes ); 
}

/*!
  Get the size of the local to global zones array
*/
void cltgzsz ( int *size ){ (*size)=0;assert( "Not implemented" ); }
/*! Fortran cltgzsz() */
void cltgzsz_( int *size ){ 
  cltgzsz( size );
}

/*!
  Get the local to global zones array
*/
void cltgz ( int *zones ){ (*zones)=0;assert( "Not implemented" ); }
/*! Fortran cltgz() */
void cltgz_( int *zones ){ 
  cltgz( zones );
}

/*!
  Get the size of the local to global edges array
*/
void cltgesz ( int *size ){ (*size)=0;assert( "Not implemented" ); }
/*! Fortran cltgesz() */
void cltgesz_( int *size ){ 
  /* Call C */
  cltgesz( size );
}

/*!
  Get the local to global edges array
*/
void cltge ( int *edges ){ (*edges)=0;assert("Not implemented"); }
/*! Fortran cltge() */
void cltge_( int *edges ){ 
/*Call C */
  cltge( edges );
}

/*!
  Get the size of the recieve faces array
  This is the size of the domain array and size of the domain
  face list array for example:
  [domid1, domid2, domid3] and [numfaces1, numfaces2, numfaces3]
*/ 
void crecfsz ( int *size ) {
  
#ifdef USE_MPI
  int myRank = 0;
  MPI_Comm_rank(MPI_COMM_WORLD, &myRank);
  getSendReceiveFacesSize(size,blockProcessorMap[myRank],0);
  #else
  CMGDPRINT("Function crecfsz does nothing in serial");
  (*size)=0;
#endif
  

}

/*! Fortran crecfsz() */
void crecfsz_( int *size ) { 
crecfsz( size );
}

/*!
Get the domain id array and the faces per domain array
*/
void crecfar (int *domar, int *fcszar ) {
#ifdef USE_MPI
  int myRank = 0;
  MPI_Comm_rank(MPI_COMM_WORLD,&myRank);
  getSendReceiveFacesArray(domar,fcszar,blockProcessorMap[myRank],0);
  #else
  CMGDPRINT("Function crecfar does nothing in serial");
  (*fcszar)=0; (*domar) = 0;
  
  
#endif
}
/*! Fortran crecfar() */
void crecfar_(int *domar, int *fcszar ) { 
  crecfar ( domar, fcszar ); 
}

/*!
  Get the recieve faces for the given domain
*/
void crecf ( int domid, int *faces ) {
#ifdef USE_MPI
  int myRank = 0;
  MPI_Comm_rank(MPI_COMM_WORLD,&myRank);
  getSendReceiveFaces(blockProcessorMap[myRank],domid,faces,0);
  #else
  CMGDPRINT("Function crecf does nothing in serial");
  domid = 0;
  (*faces) = 0;
#endif

}
/*! Fortran crecf() */
void crecf_( int *domid, int *faces ) { 
  crecf( (*domid), faces );
}

/*!
  Get the size of the recieve nodes array
*/
void crecnsz ( int *size ) {
#ifdef USE_MPI
  int myRank = 0;
  MPI_Comm_rank(MPI_COMM_WORLD, &myRank);
  getSendReceiveNodesSize(size,blockProcessorMap[myRank],0);
  #else
  CMGDPRINT("Function crecnsz does nothing in serial");
  (*size) = 0;
  
#endif

}
/*! Fortran crecnsz() */
void crecnsz_( int *size ) { 
  crecnsz( size );
}

/*!
  Get the domain id array and the nodes per domain array
*/
void crecnar ( int *domar, int *nszar ) {
#ifdef USE_MPI
  int myRank = 0;
  MPI_Comm_rank(MPI_COMM_WORLD,&myRank);
  getSendReceiveNodesArray(domar,nszar,blockProcessorMap[myRank],0);
  #else
  CMGDPRINT("Function crecnar does nothing in serial");
  (*domar) = 0; (*nszar) = 0;
  
#endif

}
/*! Fortran crecnar() */
void crecnar_( int *domar, int *nszar ) { 
  crecnar( domar, nszar ); 
}

/*!
  Get the receive nodes for the given domain
*/
void crecn ( int domid, int *nodes ) {
#ifdef USE_MPI
  int myRank = 0;
  MPI_Comm_rank(MPI_COMM_WORLD,&myRank);
  getSendReceiveNodes(blockProcessorMap[myRank],domid,nodes,0);
  #else
  CMGDPRINT("Function crecn does nothing in serial");
  domid = 0; (*nodes) =0 ;
  
#endif
}
/*! Fortran crecn() */
void crecn_( int *domid, int *nodes ) { 
  crecn( (*domid), nodes );
}

/*!
  Get size of the receive zones array
*/
void creczsz ( int *size ) {
#ifdef USE_MPI
  int myRank = 0;
  MPI_Comm_rank(MPI_COMM_WORLD, &myRank);
  getSendReceiveZonesSize(size,blockProcessorMap[myRank],0);
  #else
  CMGDPRINT("Function creczsz does nothing in serial");
  (*size) = 0;
  
#endif

}
/*! Fortran creczsz() */
void creczsz_( int *size ) { 
  creczsz( size );
}

/*!
  Get the domain id array and the zones per domain array
*/
void creczar ( int *domar, int *zszar ) {
#ifdef USE_MPI
  int myRank = 0;
  MPI_Comm_rank(MPI_COMM_WORLD,&myRank);
  getSendReceiveZonesArray(domar,zszar,blockProcessorMap[myRank],0);
#endif


}
/*! Fortran creczar() */
void creczar_( int *domar, int *zszar ) { 
  creczar( domar, zszar );
}

/*!
  Get the recieve zones for a given domain
*/
void crecz ( int domid, int *zones ) {
  
#ifdef USE_MPI
  int myRank = 0;
  MPI_Comm_rank(MPI_COMM_WORLD,&myRank);
  getSendReceiveZones(blockProcessorMap[myRank],domid,zones,0);
#endif

}
/*! Fortran crecz() */
void crecz_( int *domid, int *zones ) { 
  /* Call C */
  crecz( (*domid), zones );
}

/*!
  Get the size of the send faces array
*/
void csndfsz (int *size ) {
#ifdef USE_MPI
  int myRank = 0;
  MPI_Comm_rank(MPI_COMM_WORLD, &myRank);
  getSendReceiveFacesSize(size,blockProcessorMap[myRank],1);
#endif

}
/*! Fortran csndfsz() */
void csndfsz_(int *size ) { 
  csndfsz( size );
}

/*!
  Get the domain id array and the faces per domain array
*/
void csndfar ( int *domar, int *fszar ) {
#ifdef USE_MPI
  int myRank = 0;
  MPI_Comm_rank(MPI_COMM_WORLD,&myRank);
  getSendReceiveFacesArray(domar,fszar,blockProcessorMap[myRank],1);
#endif

}
/*! Fortran csndfar() */
void csndfar_( int *domar, int *fszar ) { 
  /* Call C */
  csndfar( domar, fszar );
}

/*!
  Get the send faces for the given domain
*/
void csndf (int domid, int *faces) {
#ifdef USE_MPI
  int myRank = 0;
  MPI_Comm_rank(MPI_COMM_WORLD,&myRank);
  getSendReceiveFaces(blockProcessorMap[myRank],domid,faces,1);
#endif

}
/*! Fortran csndf() */
void csndf_(int *domid, int *faces) { 
  /* Call C */
  csndf( (*domid), faces );
}

/*!
  Get the size of the send nodes array
*/
void csndnsz ( int *size ) {
#ifdef USE_MPI
  int myRank = 0;
  MPI_Comm_rank(MPI_COMM_WORLD, &myRank);
  getSendReceiveNodesSize(size,blockProcessorMap[myRank],1);
#endif

}
/*! Fortran csndnsz() */
void csndnsz_( int *size ) { 
  /* Call C */
  csndnsz( size );
}

/*!
  Get the domain id array and the nodes per domain array
*/
void csndnar (int *domar, int *nszar) {
#ifdef USE_MPI
  int myRank = 0;
  MPI_Comm_rank(MPI_COMM_WORLD,&myRank);
  getSendReceiveNodesArray(domar,nszar,blockProcessorMap[myRank],1);
#endif



}
/*! Fortran csndnar() */
void csndnar_(int *domar, int *nszar) { 
  /* Call C */
  csndnar( domar, nszar );
}

/*!
  Get the send nodes for the given domain
*/
void csndn ( int domid, int *nodes ) {
#ifdef USE_MPI
  int myRank = 0;
  MPI_Comm_rank(MPI_COMM_WORLD,&myRank);
  getSendReceiveNodes(blockProcessorMap[myRank],domid,nodes,1);
#endif

}
/*! Fortran csndn() */
void csndn_( int *domid, int *nodes ) { 
  /* Call C */
  csndn( (*domid), nodes );
}

/*!
  Get the size of the send zones array
*/
void csndzsz ( int *size ) {
#ifdef USE_MPI
  int myRank = 0;
  MPI_Comm_rank(MPI_COMM_WORLD, &myRank);
  getSendReceiveZonesSize(size,blockProcessorMap[myRank],1);
#endif

}
/*! Fortran csndzsz() */
void csndzsz_( int *size ) { 
  /* Call C */
  csndzsz( size );
}

/*!
  Get the domain id array and the zones per domain array
*/
void csndzar (int *domar, int *zszar ) {
#ifdef USE_MPI
  int myRank = 0;
  MPI_Comm_rank(MPI_COMM_WORLD,&myRank);
  getSendReceiveZonesArray(domar,zszar,blockProcessorMap[myRank],1);
#endif

}
/*! Fortran csndzar() */
void csndzar_(int *domar, int *zszar ) { 
  /* Call C */
  csndzar( domar, zszar );
}

/*!
  Get the send zones for the given array
*/
void csndz ( int domid, int *zones ) {
#ifdef USE_MPI
  int myRank = 0;
  MPI_Comm_rank(MPI_COMM_WORLD,&myRank);
  getSendReceiveZones(blockProcessorMap[myRank],domid,zones,1);
#endif

}
/*! Fortran csndz() */
void csndz_( int *domid, int *zones ) { 
  /* Call C */
  csndz( (*domid), zones );
}


/*Start Temporary Funtions*/

void crecfszT( int *size, int domainId)
{
  getSendReceiveFacesSize(size,domainId,0);
}

void crecfarT (int *domar, int *fcszar , int domainId)
{
 
  getSendReceiveFacesArray(domar,fcszar,domainId,0);
}

void crecfT ( int thisDomain, int domid, int *faces ) {
  getSendReceiveFaces(thisDomain,domid,faces,0);
  
}
void csndfszT (int *size , int domainId)
{
  getSendReceiveFacesSize(size,domainId,1);

}
void csndfarT ( int *domar, int *fszar, int domainId )
{
  getSendReceiveFacesArray(domar,fszar,domainId,1);

}

void csndfT(int thisDomain, int domid, int *faces)
{
  getSendReceiveFaces(thisDomain,domid,faces,1);
}
void creczszT ( int *size ,int domainId)
{
  getSendReceiveZonesSize(size,domainId,0);
}
void creczarT ( int *domar, int *zszar ,int domainId )
{
  getSendReceiveZonesArray(domar,zszar,domainId,0);
}

void creczT(int thisDomain, int domid, int *zones)
{
  getSendReceiveZones(thisDomain,domid,zones,0);
}
void csndzszT ( int *size , int domainId)
{
  getSendReceiveZonesSize(size,domainId,1);
}


void csndzarT (int *domar, int *zszar , int domainId)
{
  getSendReceiveZonesArray(domar,zszar,domainId,1);
}


void csndzT(int thisDomain, int domid, int *zones)
{
  getSendReceiveZones(thisDomain,domid,zones,1);
}

void csndnszT ( int *size , int domainId)
{
  getSendReceiveNodesSize(size,domainId,1);
}


void csndnarT( int *domar, int *nszar, int domainId)
{
  getSendReceiveNodesArray(domar,nszar,domainId,1);
}


void csndnT(int thisDomain, int domainId, int *nodes)
{
  getSendReceiveNodes(thisDomain,domainId,nodes,1);
}

void crecnszT ( int *size , int domainId)
{
  getSendReceiveNodesSize(size,domainId,0);

}

void crecnarT ( int *domar, int *nszar , int domainId)
{
  getSendReceiveNodesArray(domar,nszar,domainId,0);
}


void crecnT(int thisDomain, int domid, int *nodes)
{
  getSendReceiveNodes(thisDomain,domid,nodes,0);

}





/*End Temporary Functions*/

void getSendReceiveFacesSize( int *size, int domainId, int send)
{
extern MeshCellContainer cellContainer;

  MeshCell currentCell = cellContainer.cellList[domainId];
  int offset;
  
  if(send==0)
      offset = -1;
  else
      offset = 1;
  
      
    /*check on the three faces that can be higher than me*/
  int i = currentCell.i;
  int j = currentCell.j;
  int k = currentCell.k;

  if (i==-1 ||
      j==-1 ||
      k==-1)
  {
    CMGDPRINT("Bad Domain Id on crecfszT");
  }

  (*size) = 0;

  int neighbor = getCellId(i,j,k+offset);
  if(neighbor>-1){
    if(cellContainer.cellList[neighbor].i>-1)
        (*size)+=1;
  }
  neighbor = getCellId(i,j+offset,k);
  if(neighbor>-1){
    if(cellContainer.cellList[neighbor].i>-1)
        (*size)+=1;
  }
  neighbor = getCellId(i+offset,j,k);
  if(neighbor>-1){
    if(cellContainer.cellList[neighbor].i>-1)
        (*size)+=1;
  }
  
  
}

void getSendReceiveFacesArray( int *domar, int *fcszar, int domainId, int send)
{
 int counter = 0;
  
  extern MeshCellContainer cellContainer;
  extern NumZones numZones;

  int offset;
  if(send==0)
      offset = -1;
  else
      offset = 1;
  
  

  MeshCell currentCell = cellContainer.cellList[domainId];

    /*check on the three faces that can be higher than me*/
  int i = currentCell.i;
  int j = currentCell.j;
  int k = currentCell.k;

  if (i==-1 ||
      j==-1 ||
      k==-1)
  {
    CMGDPRINT("Bad Domain Id on crecfszT");
  }

  int neighbor = getCellId(i,j,k+offset);
  if(neighbor>-1){
    if(cellContainer.cellList[neighbor].i>-1){
      domar[counter] = neighbor;
      fcszar[counter] = numZones.iZones[cellContainer.cellList[neighbor].i]*
          numZones.jZones[cellContainer.cellList[neighbor].j];
      counter++;
    }   
  }
  neighbor = getCellId(i,j+offset,k);
  if(neighbor>-1){
    if(cellContainer.cellList[neighbor].i>-1){
     domar[counter] = neighbor;
     fcszar[counter] = numZones.iZones[cellContainer.cellList[neighbor].i]*
          numZones.kZones[cellContainer.cellList[neighbor].k];
      counter++; 
    }
  }
  neighbor = getCellId(i+offset,j,k);
  if(neighbor>-1){
    if(cellContainer.cellList[neighbor].i>-1){
     domar[counter] = neighbor;
     fcszar[counter] = numZones.jZones[cellContainer.cellList[neighbor].j]*
          numZones.kZones[cellContainer.cellList[neighbor].k];
      counter++; 
    }
  }
  
  
}

void getSendReceiveFaces(int thisDomain, int domid, int *faces, int send)
{
/*Ok which neighbor is it*/
  extern MeshCellContainer cellContainer;
  extern MeshFaceContainer faceContainer;
  
  MeshCell thisCell = cellContainer.cellList[thisDomain];
  MeshCell neighborCell = cellContainer.cellList[domid];
  
  int i = thisCell.i;
  int j = thisCell.j;
  int k = thisCell.k;

  int neighborI = neighborCell.i;
  int neighborJ = neighborCell.j;
  int neighborK = neighborCell.k;
  int counter = 0;
  
  int offset;
  if(send==0)
      offset = -1;
  else
      offset = 1;
  

  if (i==-1 ||
      j==-1 ||
      k==-1)
  {
    CMGDPRINT("Bad Domain Id on crecfT");
  }

    /*We need to figure out which face is shared, and then
      we just return all the id's of the face that is shared*/

  if(i+offset == neighborI){
      /*this is the neihgbor in the -i direction.*/
    MeshFace iFace = send==0 ? faceContainer.faceList[thisCell.meshFaceIds[0]] :
        faceContainer.faceList[thisCell.meshFaceIds[1]];
    getMeshFaceFaces(iFace,faces);
    
  }
  else if(j+offset == neighborJ){
      MeshFace jFace = send == 0 ? faceContainer.faceList[thisCell.meshFaceIds[2]] :
      faceContainer.faceList[thisCell.meshFaceIds[3]];
                             
    getMeshFaceFaces(jFace,faces);
    
  }
  else if(k+offset == neighborK){
    MeshFace kFace = send == 0 ? faceContainer.faceList[thisCell.meshFaceIds[4]] :
        faceContainer.faceList[thisCell.meshFaceIds[5]];
    
    getMeshFaceFaces(kFace,faces);
  }
  else
      CMGDPRINT("Bad request in crefT");
  

}

void getSendReceiveZonesSize( int *size, int domainId, int send)
{

  extern MeshCellContainer cellContainer;
  MeshCell currentCell = cellContainer.cellList[domainId];
  
  
      
    /*check on the three faces that can be higher than me*/
  int i = currentCell.i;
  int j = currentCell.j;
  int k = currentCell.k;

  if (i==-1 ||
      j==-1 ||
      k==-1)
  {
    CMGDPRINT("Bad Domain Id on crecfszT");
  }

  (*size) = 0;

  int faceNeighbors[6];
  int edgeNeighbors[12];
  int vertexNeighbors[8];

  getCellNeighbors(currentCell, faceNeighbors,
                   edgeNeighbors, vertexNeighbors);
  
/*If the id of the neighbor is less than my id, then it is a
  a domain I will recieve from.  If the domain is great than me,
  then it is a domain that I will send to
*/
  int counter;
  for(counter = 0;counter<6;++counter){
    if(faceNeighbors[counter]>=0){
      /* if(send==0 && faceNeighbors[counter] > getCellId(i,j,k)) */
/*           (*size)+=1; */
/*       else if(send ==1 && faceNeighbors[counter] < getCellId(i,j,k)) */
/*           (*size)+=1; */
      (*size) +=1;
    }
  }
  for(counter = 0;counter<12;++counter){
    if(edgeNeighbors[counter]>=0){
     /*  if(send==0 && edgeNeighbors[counter] < getCellId(i,j,k)) */
/*           (*size)+=1; */
/*       else if(send ==1 && edgeNeighbors[counter] > getCellId(i,j,k)) */
/*           (*size)+=1; */
      (*size)+=1;
      
    }
  }
  for(counter = 0;counter<8;++counter){
    if(vertexNeighbors[counter]>=0){
     /*  if(send==0 && vertexNeighbors[counter] < getCellId(i,j,k)) */
/*           (*size)+=1; */
/*       else if(send ==1 && vertexNeighbors[counter] > getCellId(i,j,k)) */
/*           (*size)+=1; */
      (*size)+=1;
      
    }
  }
  
 
}

void getSendReceiveZonesArray( int *domar, int *znszar, int domainId, int send)
{
  int counter = 0;
  
  extern MeshCellContainer cellContainer;
  extern NumZones numZones;
  
  

  MeshCell currentCell = cellContainer.cellList[domainId];

    /*check on the three faces that can be higher than me*/
  int i = currentCell.i;
  int j = currentCell.j;
  int k = currentCell.k;

  if (i==-1 ||
      j==-1 ||
      k==-1)
  {
    CMGDPRINT("Bad Domain Id on crecfszT");
  }
  int faceNeighbors[6];
  int edgeNeighbors[12];
  int vertexNeighbors[8];
  int counter2 = 0;

  getCellNeighbors(currentCell, faceNeighbors,
                   edgeNeighbors, vertexNeighbors);
  
/*If the id of the neighbor is less than my id, then it is a
  a domain I will recieve from.  If the domain is great than me,
  then it is a domain that I will send to
*/
  
  for(counter = 0;counter<6;++counter){
    if(faceNeighbors[counter]>=0){
      if(counter == 0 ||
         counter == 1)
      {
        domar[counter2] = faceNeighbors[counter];
        znszar[counter2++] = numZones.jZones[j]*numZones.kZones[k];
      }
      else if(counter==2 ||
              counter == 3)
      {
        domar[counter2] = faceNeighbors[counter];
        znszar[counter2++] = numZones.iZones[i]*numZones.kZones[k]; 
      }
      else if(counter==4 ||
              counter == 5)
      {
        domar[counter2] = faceNeighbors[counter];
        znszar[counter2++] = numZones.iZones[i]*numZones.jZones[j]; 
      } 
    }
  }
  for(counter = 0;counter<12;++counter){
    if(edgeNeighbors[counter]>=0){
      if(counter>=0 &&
         counter<4){
        domar[counter2] = edgeNeighbors[counter];
        znszar[counter2++] = numZones.iZones[i];
      }
      else if(counter>=4 &&
              counter < 8){
        domar[counter2] = edgeNeighbors[counter];
        znszar[counter2++] = numZones.jZones[j];
      }
      else{
        domar[counter2] = edgeNeighbors[counter];
        znszar[counter2++] = numZones.kZones[k];
      } 
    }
  }
  for(counter = 0;counter<8;++counter){
    if(vertexNeighbors[counter]>=0){
      domar[counter2] = vertexNeighbors[counter];
      znszar[counter2++] = 1;
    }
  }
  
 
  
}

void getSendReceiveZones(int thisDomain, int domid, int *zones, int send)
{
  extern MeshCellContainer cellContainer;
  extern NumZones numZones;
  
  
  MeshCell thisCell = cellContainer.cellList[thisDomain];
  MeshCell neighborCell = cellContainer.cellList[domid];
  
  int i = thisCell.i;
  int j = thisCell.j;
  int k = thisCell.k;

  int neighborI = neighborCell.i;
  int neighborJ = neighborCell.j;
  int neighborK = neighborCell.k;
  int counter = 0;
  
  
  if (i==-1 ||
      j==-1 ||
      k==-1)
  {
    CMGDPRINT("Bad Domain Id on crecfT");
  }

  Range iRange;
  Range jRange;
  Range kRange;

  int ii,jj,kk,base,iZones,jZones,kZones;

  if(send==0){
    iZones = numZones.iZones[neighborI];
    jZones = numZones.jZones[neighborJ];
    kZones = numZones.kZones[neighborK];
    base = neighborCell.baseZoneId;
  }
  else{
    iZones = numZones.iZones[i];
    jZones = numZones.jZones[j];
    kZones = numZones.kZones[k];

    base = thisCell.baseZoneId;
    
  }

 
  if(i==neighborI){
    iRange.min = 0;
    iRange.max = numZones.iZones[i] -1;
  }
  else if(i+1 == neighborI){
    iRange.min = 0;
    iRange.max = 0;
    if(send==1){
     iRange.min = numZones.iZones[i]-1;
     iRange.max = numZones.iZones[i]-1; 
    }
  }
  else if(i-1 == neighborI){
    iRange.min = numZones.iZones[i]-1;
    iRange.max = numZones.iZones[i]-1;
    if(send==1){
     iRange.min = 0;
     iRange.max = 0; 
    }
  }

  if(j==neighborJ){
    jRange.min = 0;
    jRange.max = numZones.jZones[j] -1;
  }
  else if(j+1 == neighborJ){
    jRange.min = 0;
    jRange.max = 0;
    if(send==1){
     jRange.min = numZones.jZones[j]-1;
     jRange.max = numZones.jZones[j]-1; 
    }
  }
  else if(j-1 == neighborJ){
    jRange.min = numZones.jZones[j]-1;
    jRange.max = numZones.jZones[j]-1;
    if(send==1){
     jRange.min = 0;
     jRange.max = 0; 
    }
  }
    

  if(k==neighborK){
    kRange.min = 0;
    kRange.max = numZones.kZones[k] -1;
  }
  else if(k+1 == neighborK){
    kRange.min = 0;
    kRange.max = 0;
    if(send==1){
     kRange.min = numZones.kZones[k]-1;
     kRange.max = numZones.kZones[k]-1; 
    }
  }
  else if(k-1 == neighborK){
    kRange.min = numZones.kZones[k]-1;
    kRange.max = numZones.kZones[k]-1;
    if(send==1){
     kRange.min = 0;
     kRange.max = 0; 
    }
  }

 
    for(ii = iRange.min ;ii<=iRange.max;++ii){
      for(jj = jRange.min;jj<=jRange.max;++jj){
        for(kk = kRange.min;kk<=kRange.max;++kk){
          zones[counter++] = kZones*jZones*ii +
              kZones*jj + kk + base;
          
        }
      }
    }
    
}


void getSendReceiveNodesSize( int *size, int domainId, int send)
{
  extern MeshCellContainer cellContainer;
  MeshCell currentCell = cellContainer.cellList[domainId];
  int i = currentCell.i;
  int j = currentCell.j;
  int k = currentCell.k;
  int ii,jj;
  int counter = 0;
  (*size) = 0;
  
  

  if (i==-1 ||
      j==-1 ||
      k==-1)
  {
    CMGDPRINT("Bad Domain Id on crecfszT");
  }

  int facesSize,edgesSize,verticesSize;
  getSendReceiveTopologyEntitiesSize(&facesSize,&edgesSize,&verticesSize,
                                     domainId,send);

  int *topologyFaces = (int*)malloc(sizeof(int) * facesSize);
  int *topologyEdges = (int*)malloc(sizeof(int) * edgesSize);
  int *topologyVertices = (int*)malloc(sizeof(int) * verticesSize);

  getSendReceiveTopologyEntities(topologyFaces,topologyEdges,topologyVertices,
                                 domainId,send);


 
  int domains[CMG_MAX_BLOCK_DEFINITIONS];

  for( ii = 0;ii<CMG_MAX_BLOCK_DEFINITIONS;++ii)
      domains[ii] = -1;

    /*now go through and populate the domains that we actually have*/


  for(ii = 0;ii<facesSize;++ii){
    if(send){
      int cellIds[2];
      getMeshCellsFromFace(cellIds,topologyFaces[ii]);
      for(jj=0;jj<2;++jj){
        if(cellIds[jj] == domainId ||
           cellIds[jj] == -1)
            continue;
        else
            domains[cellIds[jj]] = 1;
      }
    }
    else{
        /*recieive*/
      domains[getMeshCellFaceOwner(topologyFaces[ii])] = 1;
    }
  }
  for(ii=0;ii<edgesSize;++ii){
    if(send){
      int cellIds[4];
      getMeshCellsFromEdge(cellIds,topologyEdges[ii]);
      for(jj=0;jj<4;++jj){
        if(cellIds[jj] == domainId ||
           cellIds[jj] == -1)
            continue;
        else
            domains[cellIds[jj]] = 1;
      }
    }
    else
        domains[getMeshCellEdgeOwner(topologyEdges[ii])] = 1;
    
  }
  for(ii=0;ii<verticesSize;++ii){
    if(send){
      int cellIds[8];
      getMeshCellsFromVertex(cellIds,topologyVertices[ii]);
      for(jj=0;jj<8;++jj){
        if(cellIds[jj] == domainId ||
           cellIds[jj] == -1)
            continue;
        else
            domains[cellIds[jj]]=1;
      }
    }
    else
        domains[getMeshCellVertexOwner(topologyVertices[ii])] = 1;
  }
  
  free(topologyFaces);
  free(topologyEdges);
  free(topologyVertices);
  
  for( ii = 0;ii<CMG_MAX_BLOCK_DEFINITIONS;++ii){
    if(domains[ii] == 1 )
        (*size)+=1;
  }
  
}

void getSendReceiveNodesArray( int *domar, int *ndszar, int domainId, int send)
{
  
  extern MeshCellContainer cellContainer;
  extern MeshFaceContainer faceContainer;
  extern MeshEdgeContainer edgeContainer;
  extern MeshVertexContainer vertexContainer;
  
  MeshCell currentCell = cellContainer.cellList[domainId];
  int i = currentCell.i;
  int j = currentCell.j;
  int k = currentCell.k;
  int ii,jj;
  int counter = 0;
  

  if (i==-1 ||
      j==-1 ||
      k==-1)
  {
    CMGDPRINT("Bad Domain getSendReceiveNodesArray");
  }

  int facesSize,edgesSize,verticesSize;
  getSendReceiveTopologyEntitiesSize(&facesSize,&edgesSize,&verticesSize,
                                     domainId,send);

  int *topologyFaces = malloc(sizeof(int) * facesSize);
  int *topologyEdges = malloc(sizeof(int) * edgesSize);
  int *topologyVertices = malloc(sizeof(int) * verticesSize);

  getSendReceiveTopologyEntities(topologyFaces,topologyEdges,topologyVertices,
                                 domainId,send);


 
  int domains[CMG_MAX_BLOCK_DEFINITIONS];

  for( ii = 0;ii<CMG_MAX_BLOCK_DEFINITIONS;++ii)
      domains[ii] = 0;

    
  for(ii = 0;ii<facesSize;++ii){
    if(send){
      int cellIds[2];
      getMeshCellsFromFace(cellIds,topologyFaces[ii]);
      for(jj=0;jj<2;++jj){
        if(cellIds[jj] == domainId ||
           cellIds[jj] == -1)
            continue;
        else
            domains[cellIds[jj]] = domains[cellIds[jj]]+
                faceContainer.faceList[topologyFaces[ii]].numNodes;
      }
    }
    else{
        /*recieive*/
      domains[getMeshCellFaceOwner(topologyFaces[ii])] =
          domains[getMeshCellFaceOwner(topologyFaces[ii])] +
          faceContainer.faceList[topologyFaces[ii]].numNodes;
    }
  }
  for(ii=0;ii<edgesSize;++ii){
    if(send){
      int cellIds[4];
      getMeshCellsFromEdge(cellIds,topologyEdges[ii]);
      for(jj=0;jj<4;++jj){
        if(cellIds[jj] == domainId ||
           cellIds[jj] == -1)
            continue;
        else
           domains[cellIds[jj]] = domains[cellIds[jj]]+
                edgeContainer.edgeList[topologyEdges[ii]].numNodes; 
      }
    }
    else
      domains[getMeshCellEdgeOwner(topologyEdges[ii])] =
          domains[getMeshCellEdgeOwner(topologyEdges[ii])] +
          edgeContainer.edgeList[topologyEdges[ii]].numNodes;  
    
  }
  for(ii=0;ii<verticesSize;++ii){
    if(send){
      int cellIds[8];
      getMeshCellsFromVertex(cellIds,topologyVertices[ii]);
      for(jj=0;jj<8;++jj){
        if(cellIds[jj] == domainId ||
           cellIds[jj] == -1)
            continue;
        else
            domains[cellIds[jj]] = domains[cellIds[jj]]+1;
        
      }
    }
    else
        domains[getMeshCellVertexOwner(topologyVertices[ii])] =
            domains[getMeshCellVertexOwner(topologyVertices[ii])] +1;
    
  }
  
  free(topologyFaces);
  free(topologyEdges);
  free(topologyVertices);

  
  for( ii = 0;ii<CMG_MAX_BLOCK_DEFINITIONS;++ii) {
    if(domains[ii]>0){
      domar[counter] = ii;
      ndszar[counter++] = domains[ii];
    }     
  }
  
  
  


}

void getSendReceiveNodes(int thisDomain, int domid, int *nodes, int send)
{

  extern MeshCellContainer cellContainer;
  extern MeshFaceContainer faceContainer;
  extern MeshEdgeContainer edgeContainer;
  extern MeshVertexContainer vertexContainer;
  
  MeshCell currentCell = cellContainer.cellList[thisDomain];
  int i = currentCell.i;
  int j = currentCell.j;
  int k = currentCell.k;
  int ii,jj,kk;
  int counter = 0;
  

  if (i==-1 ||
      j==-1 ||
      k==-1)
  {
    CMGDPRINT("Bad Domain getSendReceiveNodesArray");
  }

  int facesSize,edgesSize,verticesSize;
  getSendReceiveTopologyEntitiesSize(&facesSize,&edgesSize,&verticesSize,
                                     thisDomain,send);

  int *topologyFaces = malloc(sizeof(int) * facesSize);
  int *topologyEdges = malloc(sizeof(int) * edgesSize);
  int *topologyVertices = malloc(sizeof(int) * verticesSize);

  getSendReceiveTopologyEntities(topologyFaces,topologyEdges,topologyVertices,
                                 thisDomain,send);

  for(ii = 0;ii<facesSize;++ii){
    if(send){
      int cellIds[2];
      getMeshCellsFromFace(cellIds,topologyFaces[ii]);
      for(jj=0;jj<2;++jj){
        if(cellIds[jj]==domid){
          for(kk=0;kk<faceContainer.faceList[topologyFaces[ii]].numNodes;
              ++kk){
            nodes[counter++] = faceContainer.faceList[topologyFaces[ii]].baseNodeId + kk;
            
          }
          break;
        }
      }
    }
    else{
        /*recieive*/
      if(getMeshCellFaceOwner(topologyFaces[ii]) == domid){
        for(kk=0;kk<faceContainer.faceList[topologyFaces[ii]].numNodes;
            ++kk){
          nodes[counter++] = faceContainer.faceList[topologyFaces[ii]].baseNodeId + kk;
            
        }
      }
    }
  }
  for(ii=0;ii<edgesSize;++ii){
    if(send){
      int cellIds[4];
      getMeshCellsFromEdge(cellIds,topologyEdges[ii]);
      for(jj=0;jj<4;++jj){
        if(cellIds[jj] == domid){
          for(kk=0;kk<edgeContainer.edgeList[topologyEdges[ii]].numNodes;
              ++kk){
            nodes[counter++] = edgeContainer.edgeList[topologyEdges[ii]].baseNodeId + kk;
            
          }
          break; 
        }   
      }
    }
    else{
      if(getMeshCellEdgeOwner(topologyEdges[ii]) == domid){
        for(kk=0;kk<edgeContainer.edgeList[topologyEdges[ii]].numNodes;
            ++kk){
          nodes[counter++] = edgeContainer.edgeList[topologyEdges[ii]].baseNodeId + kk;
        }
      }
    } 
  }
  for(ii=0;ii<verticesSize;++ii){
    if(send){
      int cellIds[8];
      getMeshCellsFromVertex(cellIds,topologyVertices[ii]);
      for(jj=0;jj<8;++jj){
        if(cellIds[jj] == domid){
          nodes[counter++] = vertexContainer.vertexList[topologyVertices[ii]].nodeId;
          break;
        }  
      }
    }
    else{
      if(getMeshCellVertexOwner(topologyVertices[ii]) == domid)
          nodes[counter++] = vertexContainer.vertexList[topologyVertices[ii]].nodeId;
    }
  } 
  
  free(topologyFaces);
  free(topologyEdges);
  free(topologyVertices);
}
