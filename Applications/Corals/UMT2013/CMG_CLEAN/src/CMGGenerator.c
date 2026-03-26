
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

This file contains the implementation of functions in cmg.h which will
create the mesh on the given domain.
*/
#include "cmg.h"
#include "dataTypes.h"
#include "CMGGenerator.h"
#include "CMGMeshTopology.h"
#include "meshAndInputData.h"
#include "cmgConstants.h"
#include "CMGDebug.h"
#include <string.h>
#include <assert.h>



#ifdef USE_MPI
#include "mpi.h"
#endif

/* malloc and free */
#include <stdlib.h>

#include <limits.h>

/*Includes for random numbers */
#include <time.h>
#include <math.h>

void cgenmesh( )
{

  int ntags;
  extern SuperMeshSize sms;
  extern MeshTagContainer meshTags;
 /*  extern BlockContainer allBlocks; */
  MeshTag tag;
  extern MeshVertexContainer vertexContainer;
  extern MeshEdgeContainer edgeContainer;
  extern MeshFaceContainer faceContainer;
  extern MeshCellContainer cellContainer;

  MeshVertexContainerInit(&vertexContainer);
  MeshEdgeContainerInit(&edgeContainer);
  MeshFaceContainerInit(&faceContainer);
  MeshCellContainerInit(&cellContainer);

  createMeshTopology( );

    /*get all Topology objects ready for mesh generations*/
  initMesh( );

  allocateZoneData( );
  
  
 /*  MeshVertexContainerPrint(&vertexContainer); */
   
/*   MeshEdgeContainerPrint(&edgeContainer); */

/*   MeshFaceContainerPrint(&faceContainer); */

/*   MeshCellContainerPrint(&cellContainer); */

    /*add default materials where needed*/
  cmtgstp(CMG_MATERIAL,&ntags);
  if(ntags==0){
    tag.meshTagType = CMG_MATERIAL;
    strcpy(tag.name,"default");

      /*zone tags are stored with
        vertex indices*/
    tag.iRange.max = sms.i;
    tag.iRange.min = 0;
    tag.jRange.max = sms.j;
    tag.jRange.min = 0;
    tag.kRange.max = sms.k;
    tag.kRange.min = 0;

    MeshTagContainerAdd(&meshTags, &tag);
  }
   
}

void setupSubdivisions( int cellContainerIndex , int iZones,
                        int jZones, int kZones)
{ 
  extern unsigned int baseSeed;
  extern MeshCellContainer cellContainer;
  extern HyperCubeSubdivisionContainer hyperSubdivisions;
  int ii, jj;
  MeshCell tempMeshCell = cellContainer.cellList[cellContainerIndex];
  double fraction = -1;
  int numberSubdividedZones =0;
  int *localZoneCheck;  

  /*first let's see if this cell is being subdivided*/
  for(ii=0;ii<hyperSubdivisions.numSubdivisions;++ii){
    int min = hyperSubdivisions.subdivisions[ii].iRange.min;
    int max = hyperSubdivisions.subdivisions[ii].iRange.max;
    if(min<=tempMeshCell.i &&
       tempMeshCell.i <= max){
      min = hyperSubdivisions.subdivisions[ii].jRange.min;
      max = hyperSubdivisions.subdivisions[ii].jRange.max;
      if(min<=tempMeshCell.j &&
         tempMeshCell.j<=max){
        min = hyperSubdivisions.subdivisions[ii].kRange.min;
        max = hyperSubdivisions.subdivisions[ii].kRange.max;
        if(min<=tempMeshCell.k &&
           tempMeshCell.k<=max){
          /*We are in the range of this subdivision
           */
          fraction = hyperSubdivisions.subdivisions[ii].fraction;
          break;
        }
      }
    }
  }
  

  /*This cell has no subdivisions directives*/
  if(fraction == -1)
    return;
    
  numberSubdividedZones = ceil((iZones*jZones*kZones * fraction));
    
  if(numberSubdividedZones<=0){
    CMGDPRINT("No zones will be subdivided. Subdivision percentage is too low\n");
    return;
  }
  localZoneCheck = malloc(sizeof(int) *(iZones*jZones*kZones));

  /*if this array has a one at the given index then this zone has already
    been marked for subdivision*/
  for(ii=0;ii<(iZones*jZones*kZones);++ii)
    localZoneCheck[ii] = 0;
    
  cellContainer.cellList[cellContainerIndex].numberSubdivisions = numberSubdividedZones;
  cellContainer.cellList[cellContainerIndex].subdivisionIds = malloc(sizeof(int) * numberSubdividedZones);

  /*Initialize*/
  for(ii=0;ii<numberSubdividedZones;++ii)
    cellContainer.cellList[cellContainerIndex].subdivisionIds[ii] = -1;

  /*init the random number generator */
  srand(baseSeed + getCellId(tempMeshCell.i,
                             tempMeshCell.j,
                             tempMeshCell.k));
  /* Loop over all the zones in the problem.
     If a zone is subdivided then it has 6 additional
     zones, 8 additional nodes, 20 additional edges,
     and 18 additional faces*/
  for (ii = 0; ii < numberSubdividedZones; ++ii) {
    
    int localZoneId = rand( )%(iZones*jZones*kZones);
    int iterationCount=0;
    
    while( (localZoneCheck[localZoneId] == 1) && (iterationCount<1000) ) /* Find a localZoneID that is not already in use. */
    {
      localZoneId = rand( )%(iZones*jZones*kZones);
      iterationCount++;
    }
    if( localZoneCheck[localZoneId] == 1 )
    {
      CMGDPRINT("CMG ERROR!  NO VALID SUBDIVIDALBE ZONE CAN BE FOUND.\n");
      exit(1);
    }
    
    int globalZoneId = localZoneId +
      cellContainer.cellList[cellContainerIndex].baseZoneId;
    
    localZoneCheck[localZoneId] = 1; /* Remove this cell from future consideration for subdivision. */
    
    cellContainer.cellList[cellContainerIndex].subdivisionIds[ii] = globalZoneId;
    

    cellContainer.cellList[cellContainerIndex].numNodes =
      cellContainer.cellList[cellContainerIndex].numNodes +8;
    cellContainer.cellList[cellContainerIndex].numEdges =
      cellContainer.cellList[cellContainerIndex].numEdges +20;
    cellContainer.cellList[cellContainerIndex].numFaces =
      cellContainer.cellList[cellContainerIndex].numFaces +18;
    cellContainer.cellList[cellContainerIndex].numZones =
      cellContainer.cellList[cellContainerIndex].numZones +6;
  }

  free(localZoneCheck);
  
}


void initMesh( )
{
/*All base offsets and id's need to be set */
  int nodeCounter = 0;
  int zoneCounter = 0;
  int faceCounter = 0;

  extern SuperMeshSize sms;
  extern MeshVertexContainer vertexContainer;
  extern MeshEdgeContainer edgeContainer;
  extern MeshFaceContainer faceContainer;
  extern MeshCellContainer cellContainer;
  extern NumZones numZones;
  
    /*vertices first*/
  int maxVertices = (sms.i+1)*(sms.j+1)*(sms.k+1);
  int i;
  for(i = 0; i<maxVertices;++i){
    if(vertexContainer.vertexList[i].i>-1)
        vertexContainer.vertexList[i].nodeId = nodeCounter++;
  }

    /*edges*/
  int maxEdges = ((sms.i)*(sms.j+1)*(sms.k+1))+
      ((sms.i+1)*sms.j*(sms.k+1))+
      ((sms.i+1)*(sms.j+1)*(sms.k));
  for(i=0;i<maxEdges;++i){
    if(edgeContainer.edgeList[i].i>-1){
      if(edgeContainer.edgeList[i].alignment == 0){
          /*i aligned edge*/
        int edgei = edgeContainer.edgeList[i].i;
        edgeContainer.edgeList[i].baseNodeId = nodeCounter;
        edgeContainer.edgeList[i].numNodes  = numZones.iZones[edgei] -1;
        nodeCounter+=edgeContainer.edgeList[i].numNodes;
      }
      else if(edgeContainer.edgeList[i].alignment ==1){
          /*j aligned edge*/
        int edgej = edgeContainer.edgeList[i].j;
        edgeContainer.edgeList[i].baseNodeId = nodeCounter;
        edgeContainer.edgeList[i].numNodes  = numZones.jZones[edgej] -1;
        nodeCounter+=edgeContainer.edgeList[i].numNodes;
      }
      else if(edgeContainer.edgeList[i].alignment ==2){
        int edgek = edgeContainer.edgeList[i].k;
        edgeContainer.edgeList[i].baseNodeId = nodeCounter;
        edgeContainer.edgeList[i].numNodes  = numZones.kZones[edgek] -1;
        nodeCounter+=edgeContainer.edgeList[i].numNodes;
      }
      else{
        assert(1);
      }
    }
  }

    /*faces*/
  int maxFaces = ((sms.i+1)*sms.j*sms.k)+
      (sms.i*(sms.j+1)*sms.k)+
      (sms.i*sms.j*(sms.k+1));
  for(i=0;i<maxFaces;++i){
    if(faceContainer.faceList[i].i>-1){
      if(faceContainer.faceList[i].alignment ==0){
          /*i normal faces*/
        int facej = faceContainer.faceList[i].j;
        int facek = faceContainer.faceList[i].k;
        faceContainer.faceList[i].baseNodeId = nodeCounter;
        faceContainer.faceList[i].numNodes =
            ((numZones.jZones[facej]-1)*(numZones.kZones[facek]-1));
        nodeCounter+=faceContainer.faceList[i].numNodes;

        faceContainer.faceList[i].baseFaceId = faceCounter;
        faceContainer.faceList[i].numFaces =
            ((numZones.jZones[facej]) * numZones.kZones[facek]);
        faceCounter += faceContainer.faceList[i].numFaces;
        
      }
      else if(faceContainer.faceList[i].alignment==1){
          /*j normal faces*/
        int facei = faceContainer.faceList[i].i;
        int facek = faceContainer.faceList[i].k;
        faceContainer.faceList[i].baseNodeId = nodeCounter;
        faceContainer.faceList[i].numNodes =
            ((numZones.iZones[facei]-1)*(numZones.kZones[facek]-1));
        nodeCounter+=faceContainer.faceList[i].numNodes;

        faceContainer.faceList[i].baseFaceId = faceCounter;
        faceContainer.faceList[i].numFaces =
            (numZones.iZones[facei] * numZones.kZones[facek]);
        faceCounter += faceContainer.faceList[i].numFaces;
        
        
      }
      else if(faceContainer.faceList[i].alignment==2){
          /*k normal faces*/
        int facei = faceContainer.faceList[i].i;
        int facej = faceContainer.faceList[i].j;
        faceContainer.faceList[i].baseNodeId = nodeCounter;
        faceContainer.faceList[i].numNodes =
            ((numZones.iZones[facei]-1)*(numZones.jZones[facej]-1));
        nodeCounter+=faceContainer.faceList[i].numNodes;

        faceContainer.faceList[i].baseFaceId = faceCounter;
        faceContainer.faceList[i].numFaces =
            (numZones.iZones[facei] * numZones.jZones[facej]);
        faceCounter += faceContainer.faceList[i].numFaces;
      }
      else{
        assert(1);
      }
    } 
  }

    /*cells*/
    /*If there are subdivisions on this cell, then we need to account for it*/
  int maxCells = sms.i*sms.j*sms.k;
  for(i=0;i<maxCells;++i){
    if(cellContainer.cellList[i].i>-1){
      int iZones = numZones.iZones[cellContainer.cellList[i].i];
      int jZones = numZones.jZones[cellContainer.cellList[i].j];
      int kZones = numZones.kZones[cellContainer.cellList[i].k];

      cellContainer.cellList[i].baseZoneId = zoneCounter;
      cellContainer.cellList[i].numZones =
          cellContainer.cellList[i].numStructuredZones =
          iZones*jZones*kZones;

      cellContainer.cellList[i].baseNodeId = nodeCounter;
      cellContainer.cellList[i].numStructuredNodes =
          cellContainer.cellList[i].numNodes =
          ((iZones-1)*(jZones-1)*(kZones-1));

       /*faces, only internal ones so we subtract one
        from the direction we are traversing*/
      cellContainer.cellList[i].baseFaceId = faceCounter;
      cellContainer.cellList[i].numFaces =
          (iZones -1 * jZones * kZones) +
          (iZones * jZones -1 * kZones) +
          (iZones * jZones * kZones -1);

      setupSubdivisions(i,iZones,jZones,kZones);

      
      zoneCounter+=cellContainer.cellList[i].numZones;
      nodeCounter+=cellContainer.cellList[i].numNodes;
      faceCounter+=cellContainer.cellList[i].numFaces;
    }
  }

}

int calculateBlockFromIJK(int i, int j, int k )
{
  extern SuperMeshSize sms;

  return (i*(sms.j)*(sms.k))+(j*(sms.k))+k;
}


void sendInput( ) {
    /* FIXME send the original input file */

    /*Take the current number of seconds since the epoch as the seed to our
      random number generator.
    */
  unsigned int seed = time(NULL);
  
    /* Add a seed statement to the input if there isn't one already */

  extern unsigned int baseSeed;

  if (baseSeed == UINT_MAX) {
    baseSeed = seed;
  }

}

void bcastinp( )
{
    /*Send the input to all processors*/

    /*Data should have been read in by the parser*/
#ifdef USE_MPI
  int myRank;
  int numProcs;
  extern SuperMeshSize sms;
  extern NumZones numZones;
  extern SubBlockContainer blocks;
  extern int blockProcessorMap[CMG_MAX_BLOCK_DEFINITIONS];
  extern MeshTagContainer meshTags;
  extern SubdivisionContainer subdivisions;
  extern HyperCubeSubdivisionContainer hyperSubdivisions;
  extern unsigned int baseSeed;
  
  
  
  
  
  
  int super[3];
  int i;
  int subblock[7];
  int tag[8];
  char tagName[CMG_MAX_NAME_SIZE];
  int subdivision[6];
  
  
  
  
  
  
  

  
  MPI_Comm_rank(MPI_COMM_WORLD, &myRank);

  MPI_Comm_size(MPI_COMM_WORLD, &numProcs);
  

  if(myRank==0){
    super[0] = sms.i;
    super[1] = sms.j;
    super[2] = sms.k;
  }
  
  MPI_Bcast( &super,3,MPI_INT,0,MPI_COMM_WORLD);

  if(myRank >0){
    sms.i = super[0];
    sms.j = super[1];
    sms.k = super[2];
  }


  CMGDPRINT("Broadcast from processor %d\n",myRank);
  CMGDPRINT("SMS i:%d j:%d k:%d\n",sms.i,sms.j,sms.k);

    /*send the num zone data*/
  if(myRank>0){
    numZones.iZones = malloc(sizeof(int) * sms.i);
    numZones.jZones = malloc(sizeof(int) * sms.j);
    numZones.kZones = malloc(sizeof(int) * sms.k);
  }

  MPI_Bcast(numZones.iZones,sms.i,MPI_INT,0,MPI_COMM_WORLD);
  MPI_Bcast(numZones.jZones,sms.j,MPI_INT,0,MPI_COMM_WORLD);
  MPI_Bcast(numZones.kZones,sms.k,MPI_INT,0,MPI_COMM_WORLD);

  CMGDPRINT("NumZoneData being recieved\n");
  NumZonesPrint(&numZones);

    /*broadcast the subblock container*/
    /*put each subblock into an array and broadcast it*/
  MPI_Bcast(&blocks.numBlocks,1,MPI_INT,0,MPI_COMM_WORLD);
  
  for(i = 0;i<blocks.numBlocks;++i){
    if(myRank==0){
      SubBlock temp = blocks.subBlocks[i];
      subblock[0] = temp.on;
      subblock[1] = temp.iRange.min;
      subblock[2] = temp.iRange.max;
      subblock[3] = temp.jRange.min;
      subblock[4] = temp.jRange.max;
      subblock[5] = temp.kRange.min;
      subblock[6] = temp.kRange.max;
    }
    MPI_Bcast(&subblock,7,MPI_INT,0,MPI_COMM_WORLD);
    if(myRank>0){
      blocks.subBlocks[i].on = subblock[0];
      blocks.subBlocks[i].iRange.min = subblock[1];
      blocks.subBlocks[i].iRange.max = subblock[2];
      blocks.subBlocks[i].jRange.min = subblock[3];
      blocks.subBlocks[i].jRange.max = subblock[4];
      blocks.subBlocks[i].kRange.min = subblock[5];
      blocks.subBlocks[i].kRange.max = subblock[6];
    }

    CMGDPRINT("subBlocks broadcasted\n");
    SubBlockContainerPrint(&blocks);
  }

    /*broadcast the mesh tags in a similar way*/
  MPI_Bcast(&meshTags.numMeshTags,1,MPI_INT,0,MPI_COMM_WORLD);

  for(i = 0;i<meshTags.numMeshTags;++i){
    if(myRank ==0){ 
    MeshTag temp = meshTags.meshTags[i];
    strcpy(tagName,temp.name);
    tag[0] = temp.meshTagType;
    tag[1] = temp.iRange.min;
    tag[2] = temp.iRange.max;
    tag[3] = temp.jRange.min;
    tag[4] = temp.jRange.max;
    tag[5] = temp.kRange.min;
    tag[6] = temp.kRange.max;
    tag[7] = temp.faceBaseIndex;
    }
      /*Broadcast the tag and the name*/
    MPI_Bcast(&tag,8,MPI_INT,0,MPI_COMM_WORLD);
    MPI_Bcast(&tagName,CMG_MAX_NAME_SIZE,MPI_CHAR,0,MPI_COMM_WORLD);
    
    if(myRank>0){
      meshTags.meshTags[i].meshTagType = tag[0];
      strcpy(meshTags.meshTags[i].name,tagName);
      meshTags.meshTags[i].iRange.min = tag[1];
      meshTags.meshTags[i].iRange.max = tag[2];
      meshTags.meshTags[i].jRange.min = tag[3];
      meshTags.meshTags[i].jRange.max = tag[4];
      meshTags.meshTags[i].kRange.min = tag[5];
      meshTags.meshTags[i].kRange.max = tag[6];
      meshTags.meshTags[i].faceBaseIndex = tag[7];
    } 
  }
  
    /*Broadcast the subdivisions
     */
  MPI_Bcast(&hyperSubdivisions.numSubdivisions,1,MPI_INT,0,MPI_COMM_WORLD);
  for(i=0;i<hyperSubdivisions.numSubdivisions;++i){
    if(myRank==0){
      HypercubeSubdivision tempsub = hyperSubdivisions.subdivisions[i];
      subdivision[0] = tempsub.iRange.min;
      subdivision[1] = tempsub.iRange.max;
      subdivision[2] = tempsub.jRange.min;
      subdivision[3] = tempsub.jRange.max;
      subdivision[4] = tempsub.kRange.min;
      subdivision[5] = tempsub.kRange.max;
    }
    MPI_Bcast(&subdivision,6,MPI_INT,0,MPI_COMM_WORLD);
    MPI_Bcast(&hyperSubdivisions.subdivisions[i].fraction,1,MPI_DOUBLE,0,MPI_COMM_WORLD);
    if(myRank>0)
    {
      hyperSubdivisions.subdivisions[i].iRange.min = subdivision[0];
      hyperSubdivisions.subdivisions[i].iRange.max = subdivision[1];
      hyperSubdivisions.subdivisions[i].jRange.min = subdivision[2];
      hyperSubdivisions.subdivisions[i].jRange.max = subdivision[3];
      hyperSubdivisions.subdivisions[i].kRange.min = subdivision[4];
      hyperSubdivisions.subdivisions[i].kRange.max = subdivision[5];
    }
    CMGDPRINT("Hyper imin: %d\n",subdivision[0]);
    CMGDPRINT("Hyper imax: %d\n",subdivision[1]);
    CMGDPRINT("Hyper jmin: %d\n",subdivision[2]);
    CMGDPRINT("Hyper jmax: %d\n",subdivision[3]);
    CMGDPRINT("Hyper kmin: %d\n",subdivision[4]);
    CMGDPRINT("Hyper kmax: %d\n",subdivision[5]);
    CMGDPRINT("Hyper fraction %f\n",hyperSubdivisions.subdivisions[i].fraction);
    

  }

  MPI_Bcast(&baseSeed,1,MPI_INT,0,MPI_COMM_WORLD);
  CMGDPRINT("Base seed recieved %d\n",baseSeed);
  
  
  
  
  int masterArraySize = sms.i*sms.j*sms.k;
  int *masterBlockArray = malloc(sizeof(int)*masterArraySize);
  int counter = 0;
  calculateMasterBlockArray(masterBlockArray);
  for(i =0;i<masterArraySize;++i){
    if(masterBlockArray[i] == 1){
      blockProcessorMap[counter] = i;
      counter++;
    }
  }

  free(masterBlockArray);
  

    /*dumb domain decomposition, make suer we have the same number of
      blocks as we have processors*/
  
  
  if( counter != numProcs )
    printf("counter(%d) != numProcs(%d)\n",counter,numProcs);
  
  assert(counter == numProcs);

  #else

  
  #endif 
  
}


  
