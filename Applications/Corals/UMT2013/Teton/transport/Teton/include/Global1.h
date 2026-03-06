#define DEBUG_OUT  0     /*  if 0, no debug-printouts  */
#define NOSHAPESMAX  4   /*  max # of zone-shapes  */
#define ZNODESMAX  8     /*  max # of nodes in a zone  */
#define DUMMY_ZONE -999  /*  dummy SILO zone-index  */

   FILE *logPtr;

   int numInternalZones;
   int numInternalSides;
   int numGhostZones;

typedef struct {
   int neighborID;
   int listSize;
   int* list;
} Comm;
