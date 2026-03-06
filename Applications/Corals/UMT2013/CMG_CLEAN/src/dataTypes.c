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
  // Walt Nissen
  //
  This defines the data structures need to hold the parsed data
  as well as the created data
*/

#include <stdlib.h>
#include <assert.h>
#include "CMGDebug.h"
#include "dataTypes.h"

extern SuperMeshSize sms;


void SuperMeshSizePrint(const SuperMeshSize *this) {
  CMGDPRINT("sms(%d, %d, %d)\n",this->i, this->j, this->k);
}

void RangePrint(const Range *this) { CMGDPRINT("%d:%d",this->min, this->max); }

bool RangeCheck(const Range *this) {
  bool everythingOK = true;
  if ((this->min < 0)||(this->max < 0)) {
    CMGFPRINT(stderr, "Range values must be greater than or equal to zero");
    everythingOK = false;
  }
  if (this->min > this->max) {
    CMGFPRINT(stderr, "Range minimum must be less than or equal to range maximum");
    everythingOK = false;
  }
  return everythingOK;
}
/*
void BlockPrint(const Block *this)
{
  CMGDPRINT("Block: %d BaseZoneId: %d NumZones: %d\n",this->id,this->baseZoneId,this->numZones);
  CMGDPRINT("\tBaseNodeId: %d NumNodes: %d\n",this->baseNodeId,this->numNodes);
  CMGDPRINT("\ti: %d j: %d k: %d\n",this->i,this->j,this->k);
  return;
  
}


void BlockContainerPrint(const BlockContainer *this)
{
  CMGDPRINT("BlockContainer with %d Blocks\n",this->numBlocks);
  int ii;
  for(ii=0;ii<this->numBlocks;ii++){
    CMGDPRINT("  ");BlockPrint(&this->blockList[ii]);
  }
  
}
*/

  
void SubBlockPrint(const SubBlock *this) {
  CMGDPRINT("blk(%d,",this->on); 
  RangePrint(&this->iRange);CMGDPRINT(",");RangePrint(&this->jRange);CMGDPRINT(",");
  RangePrint(&this->kRange);CMGDPRINT(")\n");

  return;
}

void SubBlockContainerPrint(const SubBlockContainer *this) {
  CMGDPRINT("SubBlockContainer with %d SubBlocks:\n",this->numBlocks);

  int ii;
  
  for (ii=0; ii < this->numBlocks; ++ii) {
    CMGDPRINT("  ");SubBlockPrint(&this->subBlocks[ii]);
  }

  return;
}

void SubBlockContainerAdd(SubBlockContainer *this, SubBlock *subBlock) {
  this->subBlocks[this->numBlocks] = (*subBlock);
  /* increment the number of blocks */
  this->numBlocks++;
}

void NumZonesPrint(const NumZones *this) {
  CMGDPRINT("numZones((");
  int ii;
  for (ii = 0; ii < sms.i; ++ii) {
    CMGDPRINT("%d, ",this->iZones[ii]);
  }
  CMGDPRINT("), (");

  for (ii = 0; ii < sms.j; ++ii) {
    CMGDPRINT("%d, ",this->jZones[ii]);
  }
  CMGDPRINT("), (");

  for (ii = 0; ii < sms.k; ++ii) {
    CMGDPRINT("%d, ",this->kZones[ii]);
  }  
  CMGDPRINT(")\n");

  return;
}

void MeshTagPrint(const MeshTag *this) {
  CMGDPRINT("tag(\"%s\",%d,(",this->name, this->meshTagType);
  RangePrint(&this->iRange);CMGDPRINT(",");RangePrint(&this->jRange);CMGDPRINT(",");
  RangePrint(&this->kRange);CMGDPRINT("))#faceBaseIndex=%d\n",this->faceBaseIndex);
  
  return;
}

void MeshTagContainerPrint(const MeshTagContainer *this) {
  CMGDPRINT("MeshTagContainer with %d MeshTags:\n",this->numMeshTags);

  int ii;
  
  for (ii=0; ii < this->numMeshTags; ++ii) {
    CMGDPRINT("  ");MeshTagPrint(&this->meshTags[ii]);
  }

  return;
}

void MeshTagContainerAdd(MeshTagContainer *this, MeshTag* tag) {
  this->meshTags[this->numMeshTags] = (*tag);
  this->numMeshTags++;
}

MeshTag* MeshTagContainerGet(MeshTagContainer *this, int id) {
  /*Assuming base 0 for now*/
  assert ((id >= 0) && (id < this->numMeshTags));
  return &(this->meshTags[id]);
}


/*Malloc's a new subdivision */
  Subdivision *SubdivisionInit(double fraction, int numHex, int numPri, int numPyr, int numTet, Subdivision *hex, Subdivision *pri, Subdivision *pyr, Subdivision *tet) {
  Subdivision *this = (Subdivision *)malloc(sizeof(Subdivision));

  this->zoneType = CMG_INVALID;

  this->fraction = fraction;

  if (this->fraction >= 0) {
    /* Note that this is the only call that depends on floating point
       arithmetic. Perhaps it should have a tiebreaker in case of close 
       calls */
    this->integerFraction = fraction * RAND_MAX;
  }
  else { this->integerFraction = 0; }

  this->numHex = numHex; this->numPri = numPri; 
  this->numPyr = numPyr; this->numTet = numTet;

  /* if the number is -1, we need to copy child for that type, else NULL */
  if  (this->numHex < 0) { this->hex = hex; }
  else { this->hex = NULL; }
  if (this->numPri < 0) { this->pri = pri; }
  else { this->pri = NULL; }
  if (this->numPyr < 0) { this->pyr = pyr; }
  else { this->pyr = NULL; }
  if (this->numTet < 0) { this->tet = tet; }
  else { this->tet = NULL; }
  

  this->subNodesStart = -1;

  /* Init to NULL for now, we need to figure out our subdivision type
     first */
  this->currentLevelNodeIds = NULL;

  return this;
}
 
/*! Get the pattern for this subdivision */
SubdivisionPattern SubdivisionGetPattern(const Subdivision *this) {
  /*All patterns are unique except for prisms and pyramids, which can both be 
    subdivided into pyramids and tets
  */
  /*
    subs = (
    #hex
    (0,0,6,0), (0,4,2,0), (2,4,0,0), (7,0,0,0),
    #pri
    #not unique (0,0,3,2),
    (0,3,0,2), (0,5,0,0),
    #pyr
    #not unique (0,0,1,4),
    (0,1,2,2), (1,0,4,0),
    )

    types = ["CMG_HEX_INTO_PYR", "CMG_HEX_INTO_PRI_AND_PYR",
	       "CMG_HEX_INTO_HEX_AND_PRI", "CMG_HEX_INTO_HEX",
	       #Not unique "CMG_PRI_INTO_PYR_AND_TET",
	       "CMG_PRI_INTO_PRI_AND_TET", "CMG_PRI_INTO_PRI", 
	       #not uinique "CMG_PYR_INTO_PYR_AND_TET", 
	       "CMG_PYR_INTO_PRI_AND_PYR_AND_TET","CMG_PYR_INTO_HEX_AND_PYR"]

    for sub, zoneType in zip(subs,types): print \
    "else if \
     ((((this->numHex == -1)&&(%d != 0))||(this->numHex == %d))&&\n\
      (((this->numPri == -1)&&(%d != 0))||(this->numPri == %d))&&\n\
      (((this->numPyr == -1)&&(%d != 0))||(this->numPyr == %d))&&\n\
      (((this->numTet == -1)&&(%d != 0))||(this->numTet == %d))) \n\
     { return %s; }"%(reduce(lambda x,y: x+(y, y),sub,())+(zoneType,))
  
*/
if      ((((this->numHex == -1)&&(0 != 0))||(this->numHex == 0))&&
      (((this->numPri == -1)&&(0 != 0))||(this->numPri == 0))&&
      (((this->numPyr == -1)&&(6 != 0))||(this->numPyr == 6))&&
      (((this->numTet == -1)&&(0 != 0))||(this->numTet == 0)))
     { return CMG_HEX_INTO_PYR; }
else if      ((((this->numHex == -1)&&(0 != 0))||(this->numHex == 0))&&
      (((this->numPri == -1)&&(4 != 0))||(this->numPri == 4))&&
      (((this->numPyr == -1)&&(2 != 0))||(this->numPyr == 2))&&
      (((this->numTet == -1)&&(0 != 0))||(this->numTet == 0)))
     { return CMG_HEX_INTO_PRI_AND_PYR; }
else if      ((((this->numHex == -1)&&(2 != 0))||(this->numHex == 2))&&
      (((this->numPri == -1)&&(4 != 0))||(this->numPri == 4))&&
      (((this->numPyr == -1)&&(0 != 0))||(this->numPyr == 0))&&
      (((this->numTet == -1)&&(0 != 0))||(this->numTet == 0)))
     { return CMG_HEX_INTO_HEX_AND_PRI; }
else if      ((((this->numHex == -1)&&(7 != 0))||(this->numHex == 7))&&
      (((this->numPri == -1)&&(0 != 0))||(this->numPri == 0))&&
      (((this->numPyr == -1)&&(0 != 0))||(this->numPyr == 0))&&
      (((this->numTet == -1)&&(0 != 0))||(this->numTet == 0)))
     { return CMG_HEX_INTO_HEX; }
else if      ((((this->numHex == -1)&&(0 != 0))||(this->numHex == 0))&&
      (((this->numPri == -1)&&(3 != 0))||(this->numPri == 3))&&
      (((this->numPyr == -1)&&(0 != 0))||(this->numPyr == 0))&&
      (((this->numTet == -1)&&(2 != 0))||(this->numTet == 2)))
     { return CMG_PRI_INTO_PRI_AND_TET; }
else if      ((((this->numHex == -1)&&(0 != 0))||(this->numHex == 0))&&
      (((this->numPri == -1)&&(5 != 0))||(this->numPri == 5))&&
      (((this->numPyr == -1)&&(0 != 0))||(this->numPyr == 0))&&
      (((this->numTet == -1)&&(0 != 0))||(this->numTet == 0)))
     { return CMG_PRI_INTO_PRI; }
else if      ((((this->numHex == -1)&&(0 != 0))||(this->numHex == 0))&&
      (((this->numPri == -1)&&(1 != 0))||(this->numPri == 1))&&
      (((this->numPyr == -1)&&(2 != 0))||(this->numPyr == 2))&&
      (((this->numTet == -1)&&(2 != 0))||(this->numTet == 2)))
     { return CMG_PYR_INTO_PRI_AND_PYR_AND_TET; }
else if      ((((this->numHex == -1)&&(1 != 0))||(this->numHex == 1))&&
      (((this->numPri == -1)&&(0 != 0))||(this->numPri == 0))&&
      (((this->numPyr == -1)&&(4 != 0))||(this->numPyr == 4))&&
      (((this->numTet == -1)&&(0 != 0))||(this->numTet == 0)))
     { return CMG_PYR_INTO_HEX_AND_PYR; }


/* Begin human-generated code */
if      ((((this->numHex == -1)&&(0 != 0))||(this->numHex == 0))&& 
      (((this->numPri == -1)&&(0 != 0))||(this->numPri == 0))&&
      (((this->numPyr == -1)&&(3 != 0))||(this->numPyr == 3))&&
      (((this->numTet == -1)&&(2 != 0))||(this->numTet == 2)))
     { 
       if (this->zoneType == CMG_PRI) { return CMG_PRI_INTO_PYR_AND_TET;}
     }

if      ((((this->numHex == -1)&&(0 != 0))||(this->numHex == 0))&& 
      (((this->numPri == -1)&&(0 != 0))||(this->numPri == 0))&&
      (((this->numPyr == -1)&&(1 != 0))||(this->numPyr == 1))&&
      (((this->numTet == -1)&&(4 != 0))||(this->numTet == 4)))
     {        
       if (this->zoneType == CMG_PYR) { return CMG_PYR_INTO_PYR_AND_TET; }
     }

 return CMG_INVALID_SUBDIVISION_PATTERN; 

}
   
void SubdivisionSetZoneType(Subdivision *this, cZoneType zoneType) {
  
  this->zoneType = zoneType;

  /* If needed recursively set zone type */
  if (this->numHex < 0) { SubdivisionSetZoneType(this->hex, CMG_HEX); }
  if (this->numPri < 0) { SubdivisionSetZoneType(this->pri, CMG_PRI); }
  if (this->numPyr < 0) { SubdivisionSetZoneType(this->pyr, CMG_PYR); }
  if (this->numTet < 0) { SubdivisionSetZoneType(this->tet, CMG_TET); }

  /* Allocate as many nodes as will be created here, this is not exactly
   clean but this is the first time we can deduce the complete sub
  type.*/
  CMGDPRINT("Allocating space for %d new nodes\n",SubdivisionNumCreatedNodes(this));
  this->currentLevelNodeIds = 
    (int *)malloc(sizeof(int)*SubdivisionNumCreatedNodes(this));

}



/* cZoneType SubdivisionCalcZoneType(const Subdivision *this) { */
/*    */
/*     subs = ( */
/*     #hex */
/*     (0,0,6,0), (0,4,2,0), (2,4,0,0), (7,0,0,0), */
/*     #pri */
/*     (0,0,3,2), (0,3,0,2), (0,5,0,0), */
/*     #pyr */
/*     (0,0,1,4), (0,1,2,2), (1,0,4,0), */
/*     ) */

/*     types = ['CMG_HEX']*4 + ['CMG_PRI']*3 + ['CMG_PYR']*3 */

/*     for sub, zoneType in zip(subs,types): print \ */
/*     "else if \ */
/*      ((((this->numHex == -1)&&(%d != 0))||(this->numHex == %d))&&\n\ */
/*       (((this->numPri == -1)&&(%d != 0))||(this->numPri == %d))&&\n\ */
/*       (((this->numPyr == -1)&&(%d != 0))||(this->numPyr == %d))&&\n\ */
/*       (((this->numTet == -1)&&(%d != 0))||(this->numTet == %d))) \n\ */
/*      { return %s; }"%(reduce(lambda x,y: x+(y, y),sub,())+(zoneType,)) */
/*    */

 
/*   if      ((((this->numHex == -1)&&(0 != 0))||(this->numHex == 0))&& */
/*       (((this->numPri == -1)&&(0 != 0))||(this->numPri == 0))&& */
/*       (((this->numPyr == -1)&&(6 != 0))||(this->numPyr == 6))&& */
/*       (((this->numTet == -1)&&(0 != 0))||(this->numTet == 0))) */
/*      { return CMG_HEX; } */
/* else if      ((((this->numHex == -1)&&(0 != 0))||(this->numHex == 0))&& */
/*       (((this->numPri == -1)&&(4 != 0))||(this->numPri == 4))&& */
/*       (((this->numPyr == -1)&&(2 != 0))||(this->numPyr == 2))&& */
/*       (((this->numTet == -1)&&(0 != 0))||(this->numTet == 0))) */
/*      { return CMG_HEX; } */
/* else if      ((((this->numHex == -1)&&(2 != 0))||(this->numHex == 2))&& */
/*       (((this->numPri == -1)&&(4 != 0))||(this->numPri == 4))&& */
/*       (((this->numPyr == -1)&&(0 != 0))||(this->numPyr == 0))&& */
/*       (((this->numTet == -1)&&(0 != 0))||(this->numTet == 0))) */
/*      { return CMG_HEX; } */
/* else if      ((((this->numHex == -1)&&(7 != 0))||(this->numHex == 7))&& */
/*       (((this->numPri == -1)&&(0 != 0))||(this->numPri == 0))&& */
/*       (((this->numPyr == -1)&&(0 != 0))||(this->numPyr == 0))&& */
/*       (((this->numTet == -1)&&(0 != 0))||(this->numTet == 0))) */
/*      { return CMG_HEX; } */
/* else if      ((((this->numHex == -1)&&(0 != 0))||(this->numHex == 0))&& */
/*       (((this->numPri == -1)&&(0 != 0))||(this->numPri == 0))&& */
/*       (((this->numPyr == -1)&&(3 != 0))||(this->numPyr == 3))&& */
/*       (((this->numTet == -1)&&(2 != 0))||(this->numTet == 2))) */
/*      { return CMG_PRI; } */
/* else if      ((((this->numHex == -1)&&(0 != 0))||(this->numHex == 0))&& */
/*       (((this->numPri == -1)&&(3 != 0))||(this->numPri == 3))&& */
/*       (((this->numPyr == -1)&&(0 != 0))||(this->numPyr == 0))&& */
/*       (((this->numTet == -1)&&(2 != 0))||(this->numTet == 2))) */
/*      { return CMG_PRI; } */
/* else if      ((((this->numHex == -1)&&(0 != 0))||(this->numHex == 0))&& */
/*       (((this->numPri == -1)&&(5 != 0))||(this->numPri == 5))&& */
/*       (((this->numPyr == -1)&&(0 != 0))||(this->numPyr == 0))&& */
/*       (((this->numTet == -1)&&(0 != 0))||(this->numTet == 0))) */
/*      { return CMG_PRI; } */
/* else if      ((((this->numHex == -1)&&(0 != 0))||(this->numHex == 0))&& */
/*       (((this->numPri == -1)&&(0 != 0))||(this->numPri == 0))&& */
/*       (((this->numPyr == -1)&&(1 != 0))||(this->numPyr == 1))&& */
/*       (((this->numTet == -1)&&(4 != 0))||(this->numTet == 4))) */
/*      { return CMG_PYR; } */
/* else if      ((((this->numHex == -1)&&(0 != 0))||(this->numHex == 0))&& */
/*       (((this->numPri == -1)&&(1 != 0))||(this->numPri == 1))&& */
/*       (((this->numPyr == -1)&&(2 != 0))||(this->numPyr == 2))&& */
/*       (((this->numTet == -1)&&(2 != 0))||(this->numTet == 2))) */
/*      { return CMG_PYR; } */
/* else if      ((((this->numHex == -1)&&(1 != 0))||(this->numHex == 1))&& */
/*       (((this->numPri == -1)&&(0 != 0))||(this->numPri == 0))&& */
/*       (((this->numPyr == -1)&&(4 != 0))||(this->numPyr == 4))&& */
/*       (((this->numTet == -1)&&(0 != 0))||(this->numTet == 0))) */
/*      { return CMG_PYR; } */
/*   else { */
/*     assert(1); */
    
/*   } */
   /*We never get here */
/*   return CMG_HEX; */
/* } */



/*! Check that the subdivision specified and any child subdivisions are valid. 

\param type The type of the zone (hex, pyr, etc.)
\return True if  a valid subdivison */
bool SubdivisionCheck(const Subdivision *this) {

  bool result = true;

  /*
    #Python code to generate valid subdivisions
    subs = ( 
    #hex
    (0,0,6,0), (0,4,2,0), (2,4,0,0), (7,0,0,0),
    #pri
    (0,0,3,2), (0,3,0,2), (0,5,0,0),
    #pyr
    (0,0,1,4), (0,1,2,2), (1,0,4,0),
    )
    for sub in subs: print "SubdivisionCheckHelper( this, %s )&&"%repr(sub)
  */
  switch (this->zoneType) {
  case CMG_HEX:
    /* 4 valid hex subdivisions */
    result = result && 
      SubdivisionCheckHelper( this, 0, 0, 6, 0)&&
      SubdivisionCheckHelper( this, 0, 4, 2, 0)&&
      SubdivisionCheckHelper( this, 2, 4, 0, 0)&&
      SubdivisionCheckHelper( this, 7, 0, 0, 0);
    break;

  case CMG_PRI:
    result = result &&
      SubdivisionCheckHelper( this, 0, 0, 3, 2)&&
      SubdivisionCheckHelper( this, 0, 3, 0, 2)&&
      SubdivisionCheckHelper( this, 0, 5, 0, 0);
    break;

  case CMG_PYR:
    result = result &&
      SubdivisionCheckHelper( this, 0, 0, 1, 4)&&
      SubdivisionCheckHelper( this, 0, 1, 2, 2)&&
      SubdivisionCheckHelper( this, 1, 0, 4, 0);
    break;

  case CMG_TET:
    result = false;
   break;

  default: 
    CMGFPRINT(stderr, "Invalid type %d for SubdivisionCheck", this->zoneType);
    return false;
  }

  /*Now check our child subdivisions recursively */
  if (this->numHex == -1) {
    result = result && SubdivisionCheck(this->hex);
  }
  if (this->numPri == -1) {
    result = result && SubdivisionCheck(this->pri);
  }
  if (this->numPyr == -1) {
    result = result && SubdivisionCheck(this->pyr);
  }
  if (this->numTet == -1) {
    result = result && SubdivisionCheck(this->tet);
  }
  return result ;
}

/*! Helper function that takes the number of each zone type in a valid subdivision 
  and compares it to the given subdivision. If the subdivision's value is -1, that 
  means it is going to be subdivided anyway, and so any nonzero value for numType is OK */
bool SubdivisionCheckHelper(const Subdivision *this, int numHex, int numPri, int numPyr, int numTet) {
  bool result = 
    (((this->numHex == -1)&&(numHex != 0))||(this->numHex == numHex))&&
    (((this->numPri == -1)&&(numPri != 0))||(this->numPri == numPri))&&
    (((this->numPyr == -1)&&(numPyr != 0))||(this->numPyr == numPyr))&&
    (((this->numTet == -1)&&(numTet != 0))||(this->numTet == numTet));
  return result;
}

/*! Get the number of nodes created by this subdivision.

For example, dividing a hex into 6 pyramids creates 1 new node at this level. */
int SubdivisionNumCreatedNodes(const Subdivision *this) {
  
  /*
  types = ["CMG_HEX_INTO_PYR", "CMG_HEX_INTO_PRI_AND_PYR",
	       "CMG_HEX_INTO_HEX_AND_PRI", "CMG_HEX_INTO_HEX",
	       "CMG_PRI_INTO_PYR_AND_TET",
	       "CMG_PRI_INTO_PRI_AND_TET", "CMG_PRI_INTO_PRI", 
	       "CMG_PYR_INTO_PYR_AND_TET", 
	       "CMG_PYR_INTO_PRI_AND_PYR_AND_TET","CMG_PYR_INTO_HEX_AND_PYR"]

    for pat in types: print "case %s:\n\treturn ;\nbreak;\n\n"%pat
  */
  switch(SubdivisionGetPattern(this)) {

  case CMG_HEX_INTO_PYR:
    return 1;
    break;

  case CMG_HEX_INTO_PRI_AND_PYR:
    return 2;
    break;
    
  case CMG_HEX_INTO_HEX_AND_PRI:
    return 4;
    break;

  case CMG_HEX_INTO_HEX:
    return 8;
    break;

  case CMG_PRI_INTO_PYR_AND_TET:
    return 1;
    break;
    
  case CMG_PRI_INTO_PRI_AND_TET:
    return 3;
    break;
    
  case CMG_PRI_INTO_PRI:
    return 3;
    break;
    
  case CMG_PYR_INTO_PYR_AND_TET:
    return 1;
    break;
    
  case CMG_PYR_INTO_PRI_AND_PYR_AND_TET:
    return 2;
    break;
    
  case CMG_PYR_INTO_HEX_AND_PYR:
    return 4;
    break;
 
  default:
    assert(0);
    return -1;
  }
}

/*! Get the number of nodes created by this subdivision and all child subdivisions. */
int SubdivisionNumChildNodes(const Subdivision *this) {
  
  /* Calculate the number of nodes at this subdivision level */
  int numChildNodes = SubdivisionNumCreatedNodes(this);

  /* If there's a sub-subdivision, add those nodes too */
  if (this->numHex < 0) { numChildNodes += SubdivisionNumChildNodes(this->hex); }
  if (this->numPri < 0) { numChildNodes += SubdivisionNumChildNodes(this->pri); }
  if (this->numPyr < 0) { numChildNodes += SubdivisionNumChildNodes(this->pyr); }
  if (this->numTet < 0) { numChildNodes += SubdivisionNumChildNodes(this->tet); }

  return numChildNodes;
}

/*! Get the number of zones created by this subdivision.

For example, dividing a hex into 6 pyramids creates 6 new zones at this level. */
int SubdivisionNumCreatedZones(const Subdivision *this) {
  switch(SubdivisionGetPattern(this)) {

  case CMG_HEX_INTO_PYR:
    return 6;
    break;

  case CMG_HEX_INTO_PRI_AND_PYR:
    return 6;
    break;
    
  case CMG_HEX_INTO_HEX_AND_PRI:
    return 6;
    break;

  case CMG_HEX_INTO_HEX:
    return 6;
    break;

  case CMG_PRI_INTO_PYR_AND_TET:
    return 5;
    break;
    
  case CMG_PRI_INTO_PRI_AND_TET:
    return 5;
    break;
    
  case CMG_PRI_INTO_PRI:
    return 5;
    break;
    
  case CMG_PYR_INTO_PYR_AND_TET:
    return 5;
    break;
    
  case CMG_PYR_INTO_PRI_AND_PYR_AND_TET:
    return 5;
    break;
    
  case CMG_PYR_INTO_HEX_AND_PYR:
    return 5;
    break;
 
  default:
    assert(0);
    return -1;
  }
}

/*! Get the number of zones created by this subdivision and all child subdivisions. */
int SubdivisionNumChildZones(const Subdivision *this) {
    /* Calculate the number of zones at this subdivision level */
  int numChildZones = SubdivisionNumCreatedZones(this);

  /* If there's a sub-subdivision, add those zones too */
  if (this->numHex < 0) { numChildZones += SubdivisionNumChildZones(this->hex); }
  if (this->numPri < 0) { numChildZones += SubdivisionNumChildZones(this->pri); }
  if (this->numPyr < 0) { numChildZones += SubdivisionNumChildZones(this->pyr); }
  if (this->numTet < 0) { numChildZones += SubdivisionNumChildZones(this->tet); }

  return numChildZones;
}


void SubdivisionPrint(const Subdivision* this) {
  if ((this->fraction >= 0.0)&&(this->fraction <= 1.0)) {
    CMGDPRINT("sub(%f%%,(",this->fraction*100);
  }
  else {
    CMGDPRINT("(");
  }
  if (this->numHex >= 0) {
    CMGDPRINT("%d,",this->numHex);
  }
  else {
    SubdivisionPrint(this->hex); CMGDPRINT(",");
  }

  if (this->numPri >= 0) {
    CMGDPRINT("%d,",this->numPri);
  }
  else {
    SubdivisionPrint(this->pri); CMGDPRINT(",");
  }

  if (this->numPyr >= 0) {
    CMGDPRINT("%d,",this->numPyr);
  }
  else {
    SubdivisionPrint(this->pyr); CMGDPRINT(",");
  }

  if (this->numTet >= 0) {
    CMGDPRINT("%d",this->numTet);
  }
  else {
    SubdivisionPrint(this->tet); CMGDPRINT(",");
  }
  CMGDPRINT(")\n");

  /*CMGDPRINT("#sub zoneType = %d",this->zoneType);*/
 /*  if ((this->fraction >= 0.0)&&(this->fraction <= 1.0)) { */
/*     CMGDPRINT("\n"); */
/*   } */

}

void SubdivisionContainerPrint(const SubdivisionContainer* this) {

  CMGDPRINT("SubdivisionContainer with %d Subdivisions:\n",this->numSubdivisions);

  int ii;
  
  for (ii=0; ii < this->numSubdivisions; ++ii) {
    CMGDPRINT("  \n");SubdivisionPrint(&this->subdivisions[ii]);
    CMGDPRINT("# Total of %d new nodes \n",SubdivisionNumChildNodes(&this->subdivisions[ii]));
CMGDPRINT("# Total of %d new zones \n",SubdivisionNumChildZones(&this->subdivisions[ii]));
  }

  return;
}

void SubdivisionContainerAdd(SubdivisionContainer* this, Subdivision* subdivision) {

/*
  April 19,2007- BTG:  I am adding this hyper container, as a simple container.
  The heavier weight subdivision container, is more general purpose, but is not
  currently needed.  When a another subdivison type is added, then the hyper
  container will need to be removed or not used.  I am just adding this because
  it is so much simpler and straight forward then the full blown container
*/

  extern HyperCubeSubdivisionContainer hyperSubdivisions;
  HypercubeSubdivision tempHyper;
  

  if(subdivision->numHex!=7){
      CMGDPRINT("Trying to create a subdivision of other than 1 hex -> 7 hexes, not supported\n");
      return;
  }

  tempHyper.iRange = subdivision->iRange;
  tempHyper.jRange = subdivision->jRange;
  tempHyper.kRange = subdivision->kRange;

  tempHyper.fraction = subdivision->fraction;
  

  
  hyperSubdivisions.subdivisions[hyperSubdivisions.numSubdivisions] = tempHyper;
  hyperSubdivisions.numSubdivisions++;
  

  
  
  this->subdivisions[this->numSubdivisions] = (*subdivision);
  /*Recursively set the types that are being subdivided, always starting with hex */
  SubdivisionSetZoneType(&(this->subdivisions[this->numSubdivisions]), CMG_HEX);
  /*Should really free this one, because it's being copied into the array,
    but keeping track of the pointer is a pain and it's a small leak. */
  free(subdivision);
  this->numSubdivisions++;
  return;
}

void PositionInit( Position *this, double x, double y, double z) {
  this->x = x;
  this->y = y;
  this->z = z;
}

Position PositionAdd( const Position a, const Position b) {
  Position result;
  PositionInit(&result, a.x+b.x, a.y+b.y, a.z+b.z);
  return result;
}
  
/*! return a-b */
Position PositionSubtract( const Position a, const Position b) {
  return PositionAdd(a, PositionScale(b, -1.));
}

Position PositionScale( const Position a, double factor ) {
  Position result;
  PositionInit(&result, a.x*factor, a.y*factor, a.z*factor);
  return result;
}
  
void ZoneInitEmpty( Zone *this ) {
  ZoneInit(this, -1, -1, -1, -1, -1, -1, -1, -1);
}

void ZoneInit( Zone *this, int id0, int id1, int id2, int id3, int id4, int id5, int id6, int id7) {
  this->nodeIds[0] = id0; 
  this->nodeIds[1] = id1; 
  this->nodeIds[2] = id2;
  this->nodeIds[3] = id3; 
  this->nodeIds[4] = id4; 
  this->nodeIds[5] = id5; 
  this->nodeIds[6] = id6; 
  this->nodeIds[7] = id7;
}
