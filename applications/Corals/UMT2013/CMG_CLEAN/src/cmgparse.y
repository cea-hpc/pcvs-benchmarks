%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>

#include "parseStructs.h"
  extern SuperMeshSize sms;
  extern SubBlockContainer blocks;
  extern NumZones numZones;
  extern MeshTagContainer meshTags;
  extern SubdivisionContainer subdivisions;
  extern NodeDataContainer nodeData;
  extern ZoneDataContainer zoneData;
  extern unsigned int baseSeed;
  %}



/* Declare a union that lets us return a couple of different types */
%union {
  Subdivision *subdivisionVal;
  char *strVal;
  Range rangeVal;
  IntList* intListPtr;
  int intVal;
  double doubleVal;
} /* No need for ending semicolon */



%token <intVal> SEEDTOK SMSTOK BLKTOK STATE NUMZONESTOK SUBTOK TAGTOK MESHTAGTYPE MATERIALTOK INDEXMODE SUBTYPE NUMBER WORD NAME COMMENT
%type <rangeVal> range
%type <intListPtr> numberlist;
%type <subdivisionVal> subdivision;
%%

commands: /*empty*/ 
| commands command
       ;

command:
sms_set
|
blk_set
|
numzones
|
sub_set
|
tag_create
|
material_create
|
seed_set
|
comment
;

seed_set:  SEEDTOK '(' NUMBER ')'
{
  int seed = $3;
  if (seed < 0) {
    printf("Seed value must be non-negative");
    break;
  }
  unsigned int unsignedSeed = (unsigned int) seed;
  if ( unsignedSeed < UINT_MAX) {
    baseSeed = unsignedSeed;
  }
  else {
    printf("Seed value must be smaller than UINT_MAX = %d",UINT_MAX);
    break;
  }
}
;

sms_set: SMSTOK '(' NUMBER ',' NUMBER ',' NUMBER ')'
{
  if (sms.i != 0) {
    yyerror("sms has already been called");
    break;
  }

  int iSize = $3, jSize = $5, kSize = $7;

/*   printf("MAKING sms(%d,%d,%d)\n",iSize, jSize, kSize); */

  if ((iSize < 1)||(jSize < 1)||(kSize < 1)) {
    yyerror("The block size must be greater than zero.");
    break;
  }

  sms.i = iSize;
  sms.j = jSize;
  sms.k = kSize;
  numZones.iZones = (int *)malloc(sizeof(int)*sms.i);
  numZones.jZones = (int *)malloc(sizeof(int)*sms.j);
  numZones.kZones = (int *)malloc(sizeof(int)*sms.k);
  
  /*Nasty way to mark uninitialized array*/
  numZones.iZones[0] = -1;
}
;

blk_set: BLKTOK '(' STATE ',' range ',' range ',' range ')'
{
  if (!SMSExists()) { break; }

  bool on = $3;

/*   if (on) { */
/*     printf("Creating block\n"); */
/*   } else { */
/*     printf("Removing block\n"); */
/*   } */

  /* Assign ranges and perform a cursory check */
  Range iRange = $5, jRange = $7, kRange = $9;
  if (!(RangeCheck(&iRange)&&RangeCheck(&jRange)&&RangeCheck(&kRange))) {
    break;
  }  
  SubBlock block;
  block.iRange = iRange;
  block.jRange = jRange;
  block.kRange = kRange;
  block.on = on;

  SubBlockContainerAdd(&blocks, &block);

}
;

numzones: NUMZONESTOK '(' NUMBER ',' NUMBER ',' NUMBER ')'
{
  if (!SMSExists()) { break; }


  if (numZones.iZones[0] != -1) {
    fprintf(stderr, "The number of zones has already been set");
    break;
  }
  int iNumZones = $3, jNumZones = $5, kNumZones = $7;

  /* printf("Setting numzones %d %d %d",iNumZones, jNumZones, kNumZones); */

  if ((iNumZones < 0)||(jNumZones < 0)||(kNumZones < 0)) {
    fprintf(stderr, "The number of zones must be greater than or equal to zero.");
    break;
  }

  if (sms.i == 0) {
    fprintf(stderr, "Must call sms before setting numzones");
    break;
  }

  int ii;
  for (ii = 0; ii < sms.i; ++ii) {
    numZones.iZones[ii] = $3;
  }
  for (ii = 0; ii < sms.j; ++ii) {
    numZones.jZones[ii] = $5;
  }
  for (ii = 0; ii < sms.k; ++ii) {
    numZones.kZones[ii] = $7;
  }

}
|  NUMZONESTOK '(' '(' numberlist ')' ',' '(' numberlist ')' ',' '(' numberlist ')' ')'
{ 
  if (!SMSExists()) { break; }

  if (numZones.iZones[0] != -1) {
    fprintf(stderr, "The number of zones has already been set");
    break;
  }
 /*  printf("Setting numzones of ("); */
/*   printIntList($4);printf("), ("); */
/*   printIntList($8);printf("), ("); */
/*   printIntList($12);printf(")\n"); */
  if ((!copyIntListToArray( $4, sms.i, numZones.iZones))||
      (!copyIntListToArray( $8, sms.j, numZones.jZones))||
      (!copyIntListToArray($12, sms.k, numZones.kZones))) {
    fprintf(stderr, "Must have the number of zones corresponding to the size of the superblock");
    break;
  }

  /*Check and make sure they are all positive*/
  int ii;
  for (ii = 0; ii < sms.i; ++ii) {
    /* printf("%d,",numZones.iZones[ii]); */
    if (numZones.iZones[ii] < 0) {
      fprintf(stderr, "The number of I zones (%d) must be greater than or equal to zero.", numZones.iZones[ii]);
      break;
    }
  }
  for (ii = 0; ii < sms.j; ++ii) {
    if (numZones.jZones[ii] < 0) {
      fprintf(stderr, "The number of J zones (%d) must be greater than or equal to zero.",numZones.jZones[ii]);
      break;
    }
  }
  for (ii = 0; ii < sms.k; ++ii) {
    if (numZones.kZones[ii] < 0) {
      fprintf(stderr, "The number of K zones (%d) must be greater than or equal to zero.",numZones.kZones[ii]);
      break;
    }
  }
}
;

/* a number list is a non-empty list of numbers */
numberlist: NUMBER { $$ = initIntList($1, (IntList *)NULL); } 
| numberlist ',' NUMBER { $$ = initIntList( $3, (IntList *)($1)); }
;

sub_set: SUBTOK '(' NUMBER '%' ',' range ',' range ',' range ',' subdivision ')'
{
  Subdivision *subdivision = $12;

  double percentage = (double)$3;
  if (percentage <= 0) {
    fprintf(stderr, "subdivision percentage must be greater than zero");
    break;
  }
  subdivision->fraction = percentage / 100;

  /* Assign ranges and perform a cursory check */
  Range iRange = $6, jRange = $8, kRange = $10;
  if (!(RangeCheck(&iRange)&&RangeCheck(&jRange)&&RangeCheck(&kRange))) {
    break;
  }  

  subdivision->iRange = iRange;
  subdivision->jRange = jRange;
  subdivision->kRange = kRange;

  SubdivisionContainerAdd(&subdivisions, subdivision);
}
;

/* python code to generate the subdivision parsing:

#All 9 possible subdivisions, n representing a number, s a subdivision,
#plus the all-number case
types = ['nnsn', 'nssn', 'ssnn', 'snnn', 'nnss', 'nsss', 'snsn', 'nnss', 'nsns', 'nsnn', 'nnnn'] 


for type in types:
  print "| '(' ",
  for t,ii in zip(type,range(len(type))):
    if t=='n':
      print "NUMBER",
    else:
      print "subdivision",
    if ii!=3: print "','", 
  print "')' "
  print "{ $$ = SubdivisionInit(-1.0,",
  nums = ""
  ptrs = ""
  for t,ii in zip(type,range(2,2*len(type)+2,2)):
    if t=='n':
      nums += "$%d, "%ii
      ptrs += "NULL"
    else:
      nums += "-1, "
      ptrs += "$%d"%ii
    if ii!=8: 
      ptrs += ", "
  print nums, ptrs,
  print "); }"


 /* Subdivisions are always malloc'd */
subdivision: '('  NUMBER ',' NUMBER ',' subdivision ',' NUMBER ')'
{ $$ = SubdivisionInit(-1.0, $2, $4, -1, $8,  NULL, NULL, $6, NULL ); }
| '('  NUMBER ',' subdivision ',' subdivision ',' NUMBER ')'
{ $$ = SubdivisionInit(-1.0, $2, -1, -1, $8,  NULL, $4, $6, NULL ); }
| '('  subdivision ',' subdivision ',' NUMBER ',' NUMBER ')'
{ $$ = SubdivisionInit(-1.0, -1, -1, $6, $8,  $2, $4, NULL, NULL ); }
| '('  subdivision ',' NUMBER ',' NUMBER ',' NUMBER ')'
{ $$ = SubdivisionInit(-1.0, -1, $4, $6, $8,  $2, NULL, NULL, NULL ); }
| '('  NUMBER ',' subdivision ',' subdivision ',' subdivision ')'
{ $$ = SubdivisionInit(-1.0, $2, -1, -1, -1,  NULL, $4, $6, $8 ); }
| '('  subdivision ',' NUMBER ',' subdivision ',' NUMBER ')'
{ $$ = SubdivisionInit(-1.0, -1, $4, -1, $8,  $2, NULL, $6, NULL ); }
| '('  NUMBER ',' NUMBER ',' subdivision ',' subdivision ')'
{ $$ = SubdivisionInit(-1.0, $2, $4, -1, -1,  NULL, NULL, $6, $8 ); }
| '('  NUMBER ',' subdivision ',' NUMBER ',' subdivision ')'
{ $$ = SubdivisionInit(-1.0, $2, -1, $6, -1,  NULL, $4, NULL, $8 ); }
| '('  NUMBER ',' subdivision ',' NUMBER ',' NUMBER ')'
{ $$ = SubdivisionInit(-1.0, $2, -1, $6, $8,  NULL, $4, NULL, NULL ); }
| '('  NUMBER ',' NUMBER ',' NUMBER ',' NUMBER ')'
{ $$ = SubdivisionInit(-1.0, $2, $4, $6, $8,  NULL, NULL, NULL, NULL ); }
;
/*          tag     (  "fo"  ,  node     ,   (  0:1    ,  0:1    ,  0:1    )   ) */
tag_create: TAGTOK '(' NAME ',' MESHTAGTYPE ',' '(' range ',' range ',' range ')' ')' {
  /* printf("Found a tag\n"); */
  if (!SMSExists()) { break; }

  MeshTag tag;

  const char *typeString = (const char *)$5;

  if (     !strcmp(typeString, "node")) { tag.meshTagType = CMG_NODE; }
  else if (!strcmp(typeString, "edge")) { tag.meshTagType = CMG_EDGE; }
  else if (!strcmp(typeString, "face")) { tag.meshTagType = CMG_FACE; }
  else if (!strcmp(typeString, "zone")) { tag.meshTagType = CMG_ZONE; }
  else {
    fprintf(stderr, "Bad tag type");
  }

  /*Copy name into tag*/
  strcpy(tag.name, (const char *)$3);

  Range iRange = $8, jRange = $10, kRange = $12;
  if (!(RangeCheck(&iRange) && RangeCheck(&jRange) && RangeCheck(&kRange))) {
    break;
  }
  tag.iRange = iRange; tag.jRange = jRange; tag.kRange = kRange;
  tag.faceBaseIndex = -1;

  MeshTagContainerAdd(&meshTags, &tag);
}
;

material_create: MATERIALTOK '(' NAME ',' '(' range ',' range ',' range ')' ')'
{
  if (!SMSExists()) { break; }

  MeshTag tag;
  
  tag.meshTagType = CMG_MATERIAL;

  /*Copy name into tag*/
  strcpy(tag.name, (const char *)$3);
  
  Range iRange = $6, jRange = $8, kRange = $10;
  if (!(RangeCheck(&iRange) && RangeCheck(&jRange) && RangeCheck(&kRange))) {
    break;
  }
  tag.iRange = iRange; tag.jRange = jRange; tag.kRange = kRange;
  tag.faceBaseIndex = -1;

  MeshTagContainerAdd(&meshTags, &tag);

}
;

range: NUMBER ':' NUMBER
{
  $$.min = $1;
  $$.max = $3;
  /*  RangePrint( $$ ); */
}
| NUMBER 
{
  /*If only one number is specified, the min and max are both that number */
  $$.min = $1;
  $$.max = $1;
}
;

comment: COMMENT
{
  /* printf("Ignoring comment '%s'\n", $1); */
}
;

%%

int yywrap()
{
  return 1;
}

IntList *initIntList( int val, IntList *next) {
  IntList *node;
  node = (IntList *)malloc(sizeof(IntList));

  node->val = val;
  node->next = next;

  return node;
}

void printIntList( IntList  *node ) {
  printf("%d, ",node->val);
  if (node->next != NULL) {
    printIntList( node->next ) ;
  } else {
    return;
  }
}

/* Copy the contents of the list */
bool copyIntListToArray( IntList *node, int size, int *array) {
  /* Our lists come in backwards */
  int ii;
  for (ii = 0; ii < size; ++ii) {
    if (node == NULL) {
      /*fprintf(stderr, "Invalid pointer in copyIntListToArray\n");*/
      return false;
    }
    array[size-ii-1] = node->val;
    node = node->next;
    
  }
  return true;
}

/* Return false if the sms has not been created yet */
bool SMSExists(void) {
  if (sms.i == 0) {
   fprintf(stderr, "Must create the superblock with the sms command first");
   return false;
  }
  else {
    return true;
  }
}
