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
*/
/*! Files specifically used in MeshQuery for its internal use
 */

/*These functions correspond to the local ijk for the nodes in the
  given cell*/
int getNodeFromIJK(int i, int j, int k, int cellId);
int getNodeFromIJ(int i, int j, int faceId);
int getNodeFromI(int i, int edgeId);

void getNodePositionFromCell(int cellId,int nodeId,double *x, double *y, double *z);
void getNodePositionFromFace(int faceId, int nodeId,double *x, double *y, double *z);
void getNodePositionFromEdge(int edgeId, int nodeId, double *x, double *y, double *z);
void getNodePositionFromVertex(int vertexId, int nodeId,double *x, double *y, double *z);





