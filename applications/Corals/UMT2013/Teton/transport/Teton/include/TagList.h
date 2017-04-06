/********************************************************
 * 
 * Function:   TagList.h()
 * 
 * Purpose:            
 *             This file contains the structure
 *             definition of the user defined type
 *             TagList
 * 
 ********************************************************/

#ifndef     _TagList_h
#define     _TagList_h 

#include "panace.h"

#include "tag.h"
struct TagList_S { 
  int      num_tags;
  tag      *tags;
};

typedef struct TagList_S TagList;

#endif
