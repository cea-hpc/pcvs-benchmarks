#ifndef __PARSE_STRUCTS_H__
#define __PARSE_STRUCTS_H__

#include <stdio.h>
#include "cmgConstants.h"
#include "dataTypes.h"

/* This file includes structs specific to the parser. */

struct IntListStruct {
  int val;
  struct IntListStruct *next;
};

typedef struct IntListStruct IntList;

IntList* initIntList( int val, IntList *next);

void printIntList( IntList *node );

bool copyIntListToArray( IntList *node, int size, int *array);

bool SMSExists(void);


#endif  /* __PARSE_STRUCTS_H__ */
