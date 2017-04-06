/********************************************************
 * 
 * Function:   tag.h()
 * 
 * Purpose:            
 *             This file contains the structure
 *             definition of the user defined type tag
 * 
 ********************************************************/

#ifndef     _tag_h
#define     _tag_h 

#include "panace.h"

struct tag_S { 
  char     *name;
  int      type;
  int      size;
};

typedef struct tag_S tag;

#endif
