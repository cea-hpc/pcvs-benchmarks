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

  This file contains the implementation of the commands from cmg.h that will
  be used to query the tags and materials on the mesh

*/
#include <stdio.h>
#include <string.h> /* for strcpy */
#include <assert.h>
#include <malloc.h>

#include "cmg.h"
#include "CMGTagQuery.h"
#include "fortranUtilities.h"
#include "CMGMeshTopology.h"

extern SuperMeshSize sms;
extern SubBlockContainer blocks;
extern NumZones numZones;
extern MeshTagContainer meshTags;
extern SubdivisionContainer subdivisions;
extern NodeDataContainer nodeData;
extern ZoneDataContainer zoneData;

/*
  Get the number of mesh tags
*/
void cmtgs( int *numtags ) {

  /* printf("Found %d meshTags in cmtgs.\n",meshTags.numMeshTags); */
  (*numtags) = meshTags.numMeshTags;

}

void cmtgs_( int *numtags ) {
  /* Just call the C API */
  cmtgs( numtags );
}


/*! 
  Get the number of mesh tags with the given type
*/
void cmtgstp ( cMeshTagType tagtyp, int *numtags ) {
  int ii;
  int numTotalTags;
  (*numtags) = 0;
  cmtgs(&numTotalTags);
  for (ii = 0; ii < numTotalTags; ++ii) {
    if ((*(MeshTagContainerGet(&meshTags, ii))).meshTagType == tagtyp) {
      (*numtags)++;
    }
  }
}
  
/*! Fortran cmtgstp() */
void cmtgstp_( int *tagtyp, int *numtags ) {
  /* Just call the C API */
  cmtgstp( (*tagtyp), numtags );
}


/*
  Get the name of the tag at the given tag index
*/
void cmtgnm(int tagindx, char *tagnm ){

  MeshTag meshTag = *(MeshTagContainerGet(&meshTags, tagindx));
  /*meshTags.meshTags[(*tagindx) - 0];*/
  /* Copy from meshTag.name into tagnm */
  strcpy(tagnm, meshTag.name);
}

void cmtgnm_(int *tagindx, char *tagnm, int *tagnmlen, long dummylen ) {
  /* Just call the C API */
  cmtgnm( (*tagindx), tagnm );
  (*tagnmlen) = strlen(tagnm);
  fixStringF77(tagnm, CMG_MAX_NAME_SIZE);
}

/*
  Get the mesh tag type for the given index
  Tag types are 0-node, 1-edge, 2-face, 3- zone
*/
void cmtgtp(int tagindx, int *tagtyp ) {
  MeshTag meshTag = *(MeshTagContainerGet(&meshTags, tagindx));

  /* Cast our enum to an int */
  (*tagtyp) = (int)meshTag.meshTagType;
}

void cmtgtp_(int *tagindx, int *tagtyp ) {
  /* Just call the C API */
  cmtgtp((*tagindx), tagtyp );
}

/* Get the number of ids tagged by the given mesh tag */
void cmtgidsz(int tagindx, int* idssz) {
    /*NOTE: There is an assumption in the tag sizes here.
      I assume that no subdivision are made for a given element.
      When subdivisions are implemented then this will need to be
      revisited so that the correct number of nodes are returned

      Also all mesh tags should be passed in using the vertex ijk
      indicies, even for zone tags, it is just easier to deal with.
    */
  MeshTag meshTag = *(MeshTagContainerGet(&meshTags, tagindx));
  Range iRange = meshTag.iRange;
  Range jRange = meshTag.jRange;
  Range kRange = meshTag.kRange;
  int i;
  (*idssz) = 0;
  
  extern MeshCellContainer cellContainer;
  extern MeshFaceContainer faceContainer;
  extern MeshEdgeContainer edgeContainer;

  if(meshTag.meshTagType==0){
      /*node tag*/
    int *vertexList,*edgeList,*faceList,*cellList;
    int vertexListSize = getVertices(iRange,jRange,kRange,&vertexList);
    int edgeListSize = getEdges(iRange,jRange,kRange,&edgeList);
    int faceListSize = getFaces(iRange,jRange,kRange,&faceList);
    int cellListSize = getCells(iRange,jRange,kRange,&cellList);
   
    for(i=0;i<vertexListSize;++i)
        (*idssz)+=1;
    for(i=0;i<edgeListSize;++i)
        (*idssz)+= edgeContainer.edgeList[edgeList[i]].numNodes;
    for(i=0;i<faceListSize;++i)
        (*idssz)+= faceContainer.faceList[faceList[i]].numNodes;
    for(i=0;i<cellListSize;++i)
        (*idssz)+= cellContainer.cellList[cellList[i]].numNodes;
    free(vertexList);
    free(edgeList);
    free(faceList);
    free(cellList);
    
  }
  else if(meshTag.meshTagType==2){
      /*face tag*/
      /*It's possible to have face tags on the interior faces of
        a cell, but unlikely so I am commenting it out.  Face
        tags will generally only appear on a boundary
      */

    int *faceList;
    
    int faceListSize = getFaces(iRange,jRange,kRange, &faceList);
    /* int cellListSize = getCells(iRange,jRange,kRange, &cellList); */
    for(i=0;i<faceListSize;++i)
        (*idssz)+= faceContainer.faceList[faceList[i]].numFaces;
   /*  for(i=0;i<cellListSize;++i) */
/*         (*idssz)+= cellContainer.cellList[cellList[i]].numNodes; */

    free(faceList);
   /*  free(cellList); */
    
  }
  else if(meshTag.meshTagType==3||
          meshTag.meshTagType==4){
      /*zone tag*/
    int *cellList;
    int cellListSize = getCells(iRange,jRange,kRange,&cellList);

    for(i=0;i<cellListSize;++i)
        (*idssz)+=cellContainer.cellList[cellList[i]].numZones;

    free(cellList);
  }
  
}

/* Fortran version of cmtgidsz() */
void cmtgidsz_(int *tagindx, int* idssz) {
  cmtgidsz((*tagindx), idssz);
}

/*
  Get the ids of the objects for the given mesh tag
*/
void cmtgid(int tagindx, int *ids ) {
  MeshTag meshTag = *(MeshTagContainerGet(&meshTags, tagindx));
  Range iRange = meshTag.iRange;
  Range jRange = meshTag.jRange;
  Range kRange = meshTag.kRange;
  
  int i, j;
  int index=0;
  extern MeshCellContainer cellContainer;
  extern MeshFaceContainer faceContainer;
  extern MeshEdgeContainer edgeContainer;
  extern MeshVertexContainer vertexContainer;
  

  if(meshTag.meshTagType==0){
    int *vertexList, *edgeList, *faceList, *cellList;
      /* The ranges returned here are only for the topology
         entities that are owned by the domain
      */
    int vertexListSize = getVertices(iRange,jRange,kRange,&vertexList);
    int edgeListSize = getEdges(iRange,jRange,kRange,&edgeList);
    int faceListSize = getFaces(iRange,jRange,kRange,&faceList);
    int cellListSize = getCells(iRange,jRange,kRange,&cellList);

    for(i=0;i<vertexListSize;++i){
      MeshVertex vertex = vertexContainer.vertexList[vertexList[i]];
      ids[index+i] = vertex.nodeId;
    }
    index+=vertexListSize;

    for(i=0;i<edgeListSize;++i){
      MeshEdge edge = edgeContainer.edgeList[edgeList[i]];
      for(j=0;j<edge.numNodes;++j)
          ids[index+j] = edge.baseNodeId + j;
      index+=edge.numNodes;
    }
    for(i=0;i<faceListSize;++i){
      MeshFace face = faceContainer.faceList[faceList[i]];
      for(j=0;j<face.numNodes;++j)
          ids[index+j] = face.baseNodeId+j;
      index+=face.numNodes;
    }
    for(i=0;i<cellListSize;++i){
      MeshCell cell = cellContainer.cellList[cellList[i]];
      for(j=0;j<cell.numNodes;++j)
          ids[index+j] = cell.baseNodeId + j;
      index+=cell.numNodes;        
    }
    free(vertexList);
    free(edgeList);
    free(faceList);
    free(cellList);
  }
  else if(meshTag.meshTagType==2){
    int *faceList;
    int faceListSize = getFaces(iRange,jRange,kRange,&faceList);
    for(i=0;i<faceListSize;++i){
      MeshFace face = faceContainer.faceList[faceList[i]];
      for(j=0;j<face.numFaces;++j)
          ids[index+j] = face.baseFaceId+j;
      index+=face.numFaces;
    }
    free(faceList);
  }
  else if(meshTag.meshTagType==3 ||
          meshTag.meshTagType==4){
    int *cellList;
    int cellListSize = getCells(iRange,
                                jRange,
                                kRange,&cellList);
    for(i = 0;i<cellListSize;++i){
      MeshCell cell = cellContainer.cellList[cellList[i]];
      for(j=0;j<cell.numZones;++j){
        ids[index+j] = cell.baseZoneId+j;
      }
      index+=cell.numZones;
    }
    free(cellList);
    
  }
}

void cmtgid_(int *tagindx, int *ids ) {
  cmtgid((*tagindx), ids);
}

void cmtgfzsz (int zid, int *tagsz)
{
    /*This may be a potential performance hit,
      and we may need to cache in the future so
      we only run these queries once, but we'll
      cross that bridge when we get there, if it
      is indeed needed.*/

    /*Get the number of mesh tags first*/
  int numTags;
  int tagIdx, i;
  extern MeshCellContainer cellContainer;

  cmtgs(&numTags);

  for(tagIdx = 0;tagIdx<numTags;++tagIdx){
    int type;

    cmtgtp(tagIdx,&type);
    if(type ==3 ||
       type ==4){
        /*This is a zone tag, so we need to see what it's zone
          range is*/
      MeshTag tag = *(MeshTagContainerGet(&meshTags, tagIdx));
      Range iRange = tag.iRange;
      Range jRange = tag.jRange;
      Range kRange = tag.kRange;

      int *cellList;
      int cellListSize = getCells(iRange,jRange,kRange,&cellList);
      for(i = 0;i<cellListSize;++i){
        MeshCell cell = cellContainer.cellList[cellList[i]];
        if(zid>=cell.baseZoneId &&
           zid < cell.baseZoneId+cell.numZones)
            (*tagsz)+=1;
        
      }
      free(cellList);
    }
  }
}


void cmtgfzsz_(int *zid, int *tagsz)
{
  cmtgfzsz((*zid),tagsz);
}

void cmtgfz(int zid, int *tagids)
{
  int numTags;
  int tagIdx, i, numTagIds;

  int index = 0;
  cmtgfzsz(zid,&numTagIds);
  
  
  extern MeshCellContainer cellContainer;

  cmtgs(&numTags);

  for(tagIdx = 0;tagIdx<numTags;++tagIdx){
    int type;

    cmtgtp(tagIdx,&type);
    if(type ==3 ||
       type ==4){
        /*This is a zone tag, so we need to see what it's zone
          range is*/
      MeshTag tag = *(MeshTagContainerGet(&meshTags, tagIdx));
      Range iRange = tag.iRange;
      Range jRange = tag.jRange;
      Range kRange = tag.kRange;

      int *cellList;
      int cellListSize = getCells(iRange,jRange,kRange,&cellList);
      for(i = 0;i<cellListSize;++i){
        MeshCell cell = cellContainer.cellList[cellList[i]];
        if(zid>=cell.baseZoneId &&
           zid < cell.baseZoneId+cell.numZones){
          tagids[index++] = tagIdx;
        }  
      }
      free(cellList);
    }
  }
  

}

void cmtgfz_(int *zid, int *tagids)
{
  cmtgfz((*zid),tagids);
}


void getRangeList(Range range, int *list)
{
  int i;
  for(i=0;i<(range.max-range.min);++i)
      list[i] = range.min + i;
}

