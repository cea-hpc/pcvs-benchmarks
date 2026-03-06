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
  // Benjamin T. Grover, Fri Oct 21 16:11:59 PDT 2005
  //
  This defines the data structures need to hold the parsed data
  as well as the created data
*/

/*
  Take the parsed sms command and store the indicies here
  Example: sms(10,10,10)
*/
#ifndef __DATA_TYPES_H__
#define __DATA_TYPES_H__

#include "cmg.h"
#include "cmgConstants.h"

typedef struct
{
  int i;
  int j;
  int k;
} SuperMeshSize;

void SuperMeshSizePrint(const SuperMeshSize *this);

typedef struct {
  int min;
  int max;
} Range;

void RangePrint(const Range *this);
bool RangeCheck(const Range *this);

/*
  Take the parsed block commands and store each individually
  blk(on,0:3,0:2,2:2)
*/
typedef struct
{
  bool on;
  Range iRange;
  Range jRange;
  Range kRange;
} SubBlock;

void SubBlockPrint(const SubBlock *this);

typedef struct
{
  SubBlock subBlocks[CMG_MAX_BLOCK_DEFINITIONS];
  int numBlocks;
} SubBlockContainer;

void SubBlockContainerPrint(const SubBlockContainer *this);
void SubBlockContainerAdd(SubBlockContainer *this, SubBlock *subBlock);



/*
  Take the parsed number of zones per sub-cell and store them
  There will always be as much capacity as specified in the
  SuperMeshSize
  numz(3,3,3)
*/
typedef struct 
{
  int *iZones;
  int *jZones;
  int *kZones;
} NumZones;

void NumZonesPrint(const NumZones *this);

/*
  Take the parsed mesh tags and store them
*/
typedef struct
{
  cMeshTagType meshTagType;
  char name[CMG_MAX_NAME_SIZE];
  Range iRange;
  Range jRange;
  Range kRange;
  int faceBaseIndex;
} MeshTag;

void MeshTagPrint(const MeshTag *this);

typedef struct
{
  MeshTag meshTags[CMG_MAX_MESH_TAG_DEFINITIONS];
  int numMeshTags;
} MeshTagContainer;

void MeshTagContainerPrint(const MeshTagContainer *this);
void MeshTagContainerAdd(MeshTagContainer *this, MeshTag* meshTag); 
MeshTag* MeshTagContainerGet(MeshTagContainer *this, int id);

/*!
  The HyperCubeSubdivision struct contains contains information
  about the 1hex -> 7 hexs subdivision
*/
typedef struct 
{
  Range iRange;
  Range jRange;
  Range kRange;

  double fraction;
} HypercubeSubdivision;

typedef struct
{
  HypercubeSubdivision subdivisions[CMG_MAX_SUBDIVISION_DEFINITIONS];
  int numSubdivisions;  
} HyperCubeSubdivisionContainer;

/*!
  The Subdivision struct contains information about the current level of subdivision
  of hexes into unstructured nodes
*/
struct SubdivisionStruct
{
  /*! The type of zone we are subdividing */
  cZoneType zoneType;

  /*! range of valid I values. Note that these are only used by the top-level
    subdivisions */
  Range iRange;
  Range jRange;
  Range kRange;

  /*! The fraction of zones that will be subdivided */
  double fraction;
  /*! The fraction of zones that will be subdivided, expressed
    as the numerator of integerFraction/MAX_RAND */
  unsigned int integerFraction; 

  int numHex;
  int numPri;
  int numPyr;
  int numTet;

  /*! possibly NULL pointer to the recursive subdivisions below this */
  struct SubdivisionStruct *hex;
  /*! possibly NULL pointer to the recursive subdivisions below this */
  struct SubdivisionStruct *pri;
  /*! possibly NULL pointer to the recursive subdivisions below this */
  struct SubdivisionStruct *pyr;
  /*! possibly NULL pointer to the recursive subdivisions below this */
  struct SubdivisionStruct *tet;

  /*! The local node id of the start of the nodes created by this subdivision.
   */
  int subNodesStart;
  /*! Thw local zone id of the start of the nodes created by this subsivision.
   */
  int subZonesStart;

  /*!Array of the local node ids that are introduced at this level by
    this subdivision. Size is retreived by SubdivisionNumCreatedNodes() */
  int *currentLevelNodeIds; 
};

typedef struct SubdivisionStruct Subdivision;

typedef enum  {CMG_INVALID_SUBDIVISION_PATTERN = -1,
  CMG_HEX_INTO_PYR = 0, CMG_HEX_INTO_PRI_AND_PYR = 1,
  CMG_HEX_INTO_HEX_AND_PRI = 2, CMG_HEX_INTO_HEX = 3,
  CMG_PRI_INTO_PYR_AND_TET = 4, CMG_PRI_INTO_PRI_AND_TET = 5,
  CMG_PRI_INTO_PRI = 6, 
  CMG_PYR_INTO_PYR_AND_TET = 7,CMG_PYR_INTO_PRI_AND_PYR_AND_TET = 8,
  CMG_PYR_INTO_HEX_AND_PYR = 9} SubdivisionPattern;

/*! Init a subdivision and malloc it. */
Subdivision *SubdivisionInit(double fraction, int numHex, int numPri, int numPyr, int numTet, Subdivision *hex, Subdivision *pri, Subdivision *pyr,Subdivision *tet);

/*! Get the pattern for this subdivision */
SubdivisionPattern SubdivisionGetPattern(const Subdivision *this);

/* Recursively set the zone types of the subdivisions

This is required because both pyramids and prisms can be divided into pyramids and tets.

The top-level call to this must always start with CMG_HEX because we are always 
subdividing hexes at the top level.
*/
void SubdivisionSetZoneType(Subdivision *this, cZoneType zoneType);


/*! Check that the subdivision specified and any child subdivisions are valid. 
\return True if  a valid subdivison */
bool SubdivisionCheck(const Subdivision *this);

/*! Helper function that takes the number of each zone type in a valid subdivision 
  and compares it to the given subdivision */
bool SubdivisionCheckHelper(const Subdivision *this, int numHex, int numPri, int numPyr, int numTet);

/*! Get the number of nodes created by this subdivision.

For example, dividing a hex into 6 pyramids creates 1 new node at this level. */
int SubdivisionNumCreatedNodes(const Subdivision *this);

/*! Get the number of nodes created by this subdivision and all child subdivisions. */
int SubdivisionNumChildNodes(const Subdivision *this);

/*! Get the number of zones created by this subdivision.

For example, dividing a hex into 6 pyramids creates 6 new zones at this level. */
int SubdivisionNumCreatedZones(const Subdivision *this);

/*! Get the number of zones created by this subdivision and all child subdivisions. */
int SubdivisionNumChildZones(const Subdivision *this);

void SubdivisionPrint(const Subdivision* this);

typedef struct
{
  Subdivision subdivisions[CMG_MAX_SUBDIVISION_DEFINITIONS];
  int numSubdivisions;  
} SubdivisionContainer;

void SubdivisionContainerPrint(const SubdivisionContainer* this);

void SubdivisionContainerAdd(SubdivisionContainer* this, Subdivision* subdivision);

typedef struct
{
  double *nodePositions[3];
  int numNodes;
} NodeDataContainer;

typedef struct
{
  cZoneType *zoneTypes;
  int numZones;
} ZoneDataContainer;


typedef struct 
{
  double x;
  double y;
  double z;
} Position;

/*! Initialize this position with a cartesian coordinate */
void PositionInit( Position *this, double x, double y, double z);

/*! return a+b */
Position PositionAdd( const Position a, const Position b);
/*! return a-b */
Position PositionSubtract( const Position a, const Position b);
/*! return the scalar product of a*factor */
Position PositionScale( const Position a, double factor ); 


typedef struct
{
  int nodeIds[8];
} Zone;

void ZoneInitEmpty( Zone *this );
void ZoneInit( Zone *this, int id0, int id1, int id2, int id3, int id4, int id5, int id6, int id7);


#endif /*  __DATA_TYPES_H__ */
