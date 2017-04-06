
#ifndef KULLITE_HH
#define KULLITE_HH

typedef struct { 
  double x;
  double y;
  double z;
} xyz;

typedef struct { 
  int          ndimensions;
  int          npnts;
  xyz*         positions;
  int          nfaces;
  int*         faceToNodesIndex;
  int*         nodeIndices;
  int          nzones;
  int*         zoneToFacesIndex;
  int*         faceIndices;
} kullLite;

extern kullLite* readMesh();

#endif
