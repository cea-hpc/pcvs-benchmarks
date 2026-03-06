#include <stdio.h>

#include <limits.h>

/* malloc and free */
#include <stdlib.h>

#include "dataTypes.h"
#include "meshAndInputData.h"
#include "CMGMeshTopology.h"
#include "cmg.h"

#ifdef USE_MPI
#include "mpi.h"
#endif

SuperMeshSize sms;

SubBlockContainer blocks;

NumZones numZones;

MeshTagContainer meshTags;

SubdivisionContainer subdivisions;

HyperCubeSubdivisionContainer hyperSubdivisions;

NodeDataContainer nodeData;

ZoneDataContainer zoneData;

/* BlockContainer allBlocks; */

unsigned int baseSeed;

int domain;

int blockProcessorMap[CMG_MAX_BLOCK_DEFINITIONS];


void initMeshAndInputData() {

  sms.i = 0;
  sms.j = 0;
  sms.k = 0;

  blocks.numBlocks = 0;

  numZones.iZones = NULL;
  numZones.jZones = NULL;
  numZones.kZones = NULL;

  meshTags.numMeshTags = 0;

  subdivisions.numSubdivisions = 0;

  hyperSubdivisions.numSubdivisions = 0;
  

  

  nodeData.nodePositions[0] = NULL;
  nodeData.nodePositions[1] = NULL;
  nodeData.nodePositions[2] = NULL;
  nodeData.numNodes = 0;

  zoneData.zoneTypes = NULL;
  zoneData.numZones = 0;

  /* Initialize to the maxium value */
  baseSeed = UINT_MAX;
  /* Until we go //, this is 0 */
  domain = 0;
}

void cclnmd( )
{
  cleanupNodeData( );
  cleanupZoneData( );
  cleanupSubdivisionData( );
  
}

/*!Fortran clean command */
void cclnmd_( )
{
  cclnmd( );
}


void cleanupSubdivisionData( )
{
  extern MeshCellContainer cellContainer;
  int maxCells = sms.i*sms.j*sms.k;
  int i;
  for(i=0;i<maxCells;++i){
    if(cellContainer.cellList[i].numberSubdivisions > 0)
        free(cellContainer.cellList[i].subdivisionIds);
  }
  
}

void cleanupNodeData( )
{
  

}


void allocateZoneData(  )
{
  int domainId = -1;
  int totalZones = 0;
  int i;
  
  extern MeshCellContainer cellContainer;

  #ifdef USE_MPI
  extern int blockProcessorMap[CMG_MAX_BLOCK_DEFINITIONS];
  MPI_Comm_rank(MPI_COMM_WORLD, &domainId);

  totalZones = cellContainer.cellList[blockProcessorMap[domainId]].numZones;
  #else
  totalZones = cellContainer.cellList[0].numZones;
  #endif
  
  zoneData.numZones = totalZones;
  zoneData.zoneTypes = malloc(sizeof(int)*totalZones);

    /*all zones are default allocated to HEX for now*/
  for(i=0;i<zoneData.numZones;++i)
      zoneData.zoneTypes[i] = CMG_HEX;
  
}

void cleanupZoneData( )
{
  /* free(numZones.iZones); */
/*   free(numZones.jZones); */
/*   free(numZones.kZones); */
  
  free(zoneData.zoneTypes);
}

/* void calculateMaxNodeIJK(int *i, int *j, int *k) */
/* { */
 
  
/* } */



void calculateMasterBlockArray( int *masterArray )
{
 
  
  
  int ii,jj,kk,ll,blockId;
  
  
    /*This array will be used to store what IJK blocks
      are on or off*/
  
  int totalZones = 0;
    /*Lets first figure out how many zones we will have
      blocks are numbered the same as zones and nodes
      blockid = i*nj*nk + j *nk +k
     */
  
  for(ii=0;ii<blocks.numBlocks;ii++){
    SubBlock sblock = blocks.subBlocks[ii];
    if(sblock.on){
      for(jj=sblock.iRange.min;jj<=sblock.iRange.max;jj++){
        for(kk=sblock.jRange.min;kk<=sblock.jRange.max;kk++){
          for(ll=sblock.kRange.min;ll<=sblock.kRange.max;ll++){
              /*totalZones = totalZones + (numZones.iZones[jj] *
                numZones.jZones[kk] *
                numZones.kZones[ll]);
              */
            blockId = (jj*sms.j*sms.k)+(kk*sms.k) +ll;
              /*Turn on the given block*/
            masterArray[blockId]=1;
            
          }
        }
      }
    }
    else{ /*These blocks have been turned off*/
       for(jj=sblock.iRange.min;jj<=sblock.iRange.max;jj++){
        for(kk=sblock.jRange.min;kk<=sblock.jRange.max;kk++){
          for(ll=sblock.kRange.min;ll<=sblock.kRange.max;ll++){
            blockId = (jj*sms.j*sms.k)+(kk*sms.k) +ll;
              /*Turn off the given block*/
            masterArray[blockId]=0;
            
          }
        }
      }
    }
  }

}

