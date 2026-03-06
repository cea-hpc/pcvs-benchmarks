/*----------------------------------------------------------------------
Taken from DN, subZone.f

 Hexahedral zones may be divided into a variety of subzonings.  This
 will create new zone types including triangular prisms, pyramids and
 tetrahedra.  The resulting zones may be further permuted to promote
 that the widest possible range of connectivity permutations for
 testing.  This may also be used to generate load imbalances.
 NOTE: zones are divided in ways that only affect the zone being
       divided and NOT neighboring zones.  That is, original zone
       faces are not affected in any way.
 NOTE: In order to generate some tetrahedra, an additional division of
       the first subzoning is required.  In particular, triangular
       prisms and pyramids allow subdivisions which generate
       tetrahedra.
----------------------------------------------------------------------
*/
#include <assert.h>
#include <stdio.h>

#include "dataTypes.h"
#include "cmgConstants.h"

#include "subdivision.h"



Position GetPositionDummy(int nodeId) { Position p; PositionInit(&p,0,0,0); return p; }
void SetPositionDummy(int nodeId, Position p) { PositionInit(&p,0,0,0); }
void SetZoneDummy(int zoneId, Zone z) { z; }


/* subroutine hexByOne (nZ) */
void hexIntoPyr(Subdivision *this, int *nodeIdList) {
    /*----------------------------------------------------------------------
 The hexahedral zone, nZ, is divided by one new node into six pyramids.
 ----------------------------------------------------------------------*/
/*       implicit real*8 (a-h,o-z) */
/*       include 'MeshStorageDef.h' */
      
/*       integer*4 n1, n2, n3, n4, n5, n6, n7, n8 */
/*       integer*4 nc, nz1, nz2, nz3, nz4, nz5, nz6 */
      

/*     
c---------------------------------- copy original hexahedra's node list
c---------------------------------- it will be overwritten
*/

  int n1=nodeIdList[0],n2=nodeIdList[1],n3=nodeIdList[2],n4=nodeIdList[3],n5=nodeIdList[4],n6=nodeIdList[5],n7=nodeIdList[6],n8=nodeIdList[7];
  Position p1,p2,p3,p4,p5,p6,p7,p8;
  p1 = GetPositionDummy(n1);
  p2 = GetPositionDummy(n2);
  p3 = GetPositionDummy(n3);
  p4 = GetPositionDummy(n4);
  p5 = GetPositionDummy(n5);
  p6 = GetPositionDummy(n6);
  p7 = GetPositionDummy(n7);
  p8 = GetPositionDummy(n8);
    

/*----------------------------------------------- calculate the centroid*/
  Position centroid;
  int newNode = this->subNodesStart+0;
  /*Add up all the positions, then scale by 1/8th*/
  centroid = 
    PositionScale(PositionAdd(PositionAdd(PositionAdd(PositionAdd(PositionAdd(PositionAdd(PositionAdd(
												      p1, p2), p3), p4), p5), p6), p7), p8), 1./8.);
  /*      xc = xn(n1)+xn(n2)+xn(n3)+xn(n4)+xn(n5)+xn(n6)+xn(n7)+xn(n8)
	  yc = yn(n1)+yn(n2)+yn(n3)+yn(n4)+yn(n5)+yn(n6)+yn(n7)+yn(n8)
	  zc = zn(n1)+zn(n2)+zn(n3)+zn(n4)+zn(n5)+zn(n6)+zn(n7)+zn(n8)
      xn(nc) = 0.125 * xc
      yn(nc) = 0.125 * yc
      zn(nc) = 0.125 * zc

  */

  /*c--------------------------------------------- increment the node count
      numNodes = numNodes + 1
      nc       = numNodes
  */

  /*------------------------------------------ store the centroid position */
  /*WIN Store at the place where our sub-nodes start, this is the only sub-
    node created */
  SetPositionDummy(newNode, centroid);

  /*----------------------------------------- set boundary marker to "off"
      mBndN(nc) = 0
  */

  /*------------------------------------------- make the list of new zones
    c					     and increment zone counts*/
 /*      nz1 = nZ */
/*       nz2 = numZones + 1 */
/*       nz3 = numZones + 2 */
/*       nz4 = numZones + 3 */
/*       nz5 = numZones + 4 */
/*       nz6 = numZones + 5 */
      
/*       numZones = numZones + 5 */

/* ------------------------------------ store the new pyramid zone lists,
   c				      remembering to zero 6,7,8 */
  Zone newZone;

  ZoneInit(&newZone, n1, n2, n3, n4, newNode, 0, 0, 0);
  SetZoneDummy(this->subZonesStart + 0, newZone);

  ZoneInit(&newZone, n1, n5, n6, n2, newNode, 0, 0, 0);
  SetZoneDummy(this->subZonesStart + 1, newZone);

  ZoneInit(&newZone, n1, n4, n8, n5, newNode, 0, 0, 0);
  SetZoneDummy(this->subZonesStart + 2, newZone);

  ZoneInit(&newZone, n2, n6, n7, n3, newNode, 0, 0, 0);
  SetZoneDummy(this->subZonesStart + 3, newZone);

  ZoneInit(&newZone, n3, n7, n8, n4, newNode, 0, 0, 0);
  SetZoneDummy(this->subZonesStart + 4, newZone);

  ZoneInit(&newZone, n5, n8, n7, n6, newNode, 0, 0, 0);
  SetZoneDummy(this->subZonesStart + 5, newZone);

  /*
      zNodeList(1,nz1) = n1
      zNodeList(2,nz1) = n2
      zNodeList(3,nz1) = n3
      zNodeList(4,nz1) = n4
      zNodeList(5,nz1) = nc
      zNodeList(6,nz1) = 0
      zNodeList(7,nz1) = 0
      zNodeList(8,nz1) = 0

      zNodeList(1,nz2) = n1
      zNodeList(2,nz2) = n5
      zNodeList(3,nz2) = n6
      zNodeList(4,nz2) = n2
      zNodeList(5,nz2) = nc
      zNodeList(6,nz2) = 0
      zNodeList(7,nz2) = 0
      zNodeList(8,nz2) = 0

      zNodeList(1,nz3) = n1
      zNodeList(2,nz3) = n4
      zNodeList(3,nz3) = n8
      zNodeList(4,nz3) = n5
      zNodeList(5,nz3) = nc
      zNodeList(6,nz3) = 0
      zNodeList(7,nz3) = 0
      zNodeList(8,nz3) = 0

      zNodeList(1,nz4) = n2
      zNodeList(2,nz4) = n6
      zNodeList(3,nz4) = n7
      zNodeList(4,nz4) = n3
      zNodeList(5,nz4) = nc
      zNodeList(6,nz4) = 0
      zNodeList(7,nz4) = 0
      zNodeList(8,nz4) = 0

      zNodeList(1,nz5) = n3
      zNodeList(2,nz5) = n7
      zNodeList(3,nz5) = n8
      zNodeList(4,nz5) = n4
      zNodeList(5,nz5) = nc
      zNodeList(6,nz5) = 0
      zNodeList(7,nz5) = 0
      zNodeList(8,nz5) = 0

      zNodeList(1,nz6) = n5
      zNodeList(2,nz6) = n8
      zNodeList(3,nz6) = n7
      zNodeList(4,nz6) = n6
      zNodeList(5,nz6) = nc
      zNodeList(6,nz6) = 0
      zNodeList(7,nz6) = 0
      zNodeList(8,nz6) = 0
  */

}




/* The hexahedral zone is divided by two new nodes into two
 pyramids and four prisms.
----------------------------------------------------------------------
subroutine hexByTwo (nZ)*/
void hexIntoPriAndPyr(Subdivision *this, int *nodeIdList) {

  /*

      implicit real*8 (a-h,o-z)
      include 'MeshStorageDef.h'
      
      integer*4 n1, n2, n3, n4, n5, n6, n7, n8
      integer*4 nc1, nc2, nz1, nz2, nz3, nz4, nz5, nz6

*/
  /*---------------------------------- copy original hexahedra's node list
    c---------------------------------- it will be overwritten */

  int n1=nodeIdList[0],n2=nodeIdList[1],n3=nodeIdList[2],n4=nodeIdList[3],n5=nodeIdList[4],n6=nodeIdList[5],n7=nodeIdList[6],n8=nodeIdList[7];
  Position p1,p2,p3,p4,p5,p6,p7,p8;
  p1 = GetPositionDummy(n1);
  p2 = GetPositionDummy(n2);
  p3 = GetPositionDummy(n3);
  p4 = GetPositionDummy(n4);
  p5 = GetPositionDummy(n5);
  p6 = GetPositionDummy(n6);
  p7 = GetPositionDummy(n7);
  p8 = GetPositionDummy(n8);



  Position centroid1, centroid2;

  /*------------------------------------------------------ face 1 centroid */
  /*  xc1 = 0.25 * (xn(n1)+xn(n2)+xn(n3)+xn(n4))
    yc1 = 0.25 * (yn(n1)+yn(n2)+yn(n3)+yn(n4))
    zc1 = 0.25 * (zn(n1)+zn(n2)+zn(n3)+zn(n4))
  */
  centroid1 = PositionScale(PositionAdd(PositionAdd(PositionAdd(p1, p2), p3), p4),0.25);


  /*------------------------------------------------------ face 2 centroid
      xc6 = 0.25 * (xn(n5)+xn(n6)+xn(n7)+xn(n8))
      yc6 = 0.25 * (yn(n5)+yn(n6)+yn(n7)+yn(n8))
      zc6 = 0.25 * (zn(n5)+zn(n6)+zn(n7)+zn(n8))
  */
  centroid2 = PositionScale(PositionAdd(PositionAdd(PositionAdd(p5, p6), p7), p8),0.25);

  
  Position newPosition1, newPosition2;
  int newNode1 = this->subNodesStart+0;
  int newNode2 = this->subNodesStart+1;
  /*c------------------------------------ store the first new node position
    xn(nc1) = xc1 + 0.33 * (xc6 - xc1)
    yn(nc1) = yc1 + 0.33 * (yc6 - yc1)
    zn(nc1) = zc1 + 0.33 * (zc6 - zc1)
  */
  newPosition1 = PositionAdd(centroid1, 
			     PositionScale(PositionSubtract(centroid1, centroid2),0.33));
  
  SetPositionDummy(newNode1, newPosition1);
  
  /*c----------------------------------- store the second new node position
    xn(nc2) = xc1 + 0.67 * (xc6 - xc1)
    yn(nc2) = yc1 + 0.67 * (yc6 - yc1)
    zn(nc2) = zc1 + 0.67 * (zc6 - zc1)
  */
  newPosition2 = PositionAdd(centroid1, 
			     PositionScale(PositionSubtract(centroid1, centroid2),0.67)); 
  SetPositionDummy(newNode2, newPosition2);
  
  
  /*c---------------------------------------- set boundary markers to "off"
    mBndN(nc1) = 0
    mBndN(nc2) = 0
  */
  
  
  /*------------------------------------- store the new pyramid zone lists
    c				       with bases on faces 1 & 6,
    c				       remembering to zero 6,7,8
  */

  Zone newZone;

  ZoneInit(&newZone, n1, n2, n3, n4, newNode1, 0, 0, 0);
  SetZoneDummy(this->subZonesStart + 0, newZone);

  ZoneInit(&newZone, n5, n8, n7, n6, newNode2, 0, 0, 0);
  SetZoneDummy(this->subZonesStart + 1, newZone);

  /*
    zNodeList(1,nz1) = n1
    zNodeList(2,nz1) = n2
    zNodeList(3,nz1) = n3
    zNodeList(4,nz1) = n4
    zNodeList(5,nz1) = nc1
    zNodeList(6,nz1) = 0
    zNodeList(7,nz1) = 0
    zNodeList(8,nz1) = 0

      zNodeList(1,nz2) = n5
      zNodeList(2,nz2) = n8
      zNodeList(3,nz2) = n7
      zNodeList(4,nz2) = n6
      zNodeList(5,nz2) = nc2
      zNodeList(6,nz2) = 0
      zNodeList(7,nz2) = 0
      zNodeList(8,nz2) = 0
  
      c--------------------------------------- store the new prism zone lists
      c				         with bases on faces 2,3,4 & 5,
      c				         remembering to zero 7,8
  */
  ZoneInit(&newZone, n1, n2, newNode1, n5, n6, newNode2, 0, 0);
  SetZoneDummy(this->subZonesStart + 2, newZone);

  ZoneInit(&newZone, n4, n1, newNode1, n8, n5, newNode2, 0, 0);
  SetZoneDummy(this->subZonesStart + 3, newZone);

  ZoneInit(&newZone, n2, n3, newNode1, n6, n7, newNode2, 0, 0);
  SetZoneDummy(this->subZonesStart + 4, newZone);

  ZoneInit(&newZone, n3, n4, newNode1, n7, n8, newNode2, 0, 0);
  SetZoneDummy(this->subZonesStart + 5, newZone);

  /*
    zNodeList(1,nz3) = n1
      zNodeList(2,nz3) = n2
      zNodeList(3,nz3) = nc1
      zNodeList(4,nz3) = n5
      zNodeList(5,nz3) = n6
      zNodeList(6,nz3) = nc2
      zNodeList(7,nz3) = 0
      zNodeList(8,nz3) = 0

      zNodeList(1,nz4) = n4
      zNodeList(2,nz4) = n1
      zNodeList(3,nz4) = nc1
      zNodeList(4,nz4) = n8
      zNodeList(5,nz4) = n5
      zNodeList(6,nz4) = nc2
      zNodeList(7,nz4) = 0
      zNodeList(8,nz4) = 0

      zNodeList(1,nz5) = n2
      zNodeList(2,nz5) = n3
      zNodeList(3,nz5) = nc1
      zNodeList(4,nz5) = n6
      zNodeList(5,nz5) = n7
      zNodeList(6,nz5) = nc2
      zNodeList(7,nz5) = 0
      zNodeList(8,nz5) = 0

      zNodeList(1,nz6) = n3
      zNodeList(2,nz6) = n4
      zNodeList(3,nz6) = nc1
      zNodeList(4,nz6) = n7
      zNodeList(5,nz6) = n8
      zNodeList(6,nz6) = nc2
      zNodeList(7,nz6) = 0
      zNodeList(8,nz6) = 0

  */
}


/*! The triangular prism zone is divided by one new node into two
  c tetrahedral zones and three pyramids.
  c----------------------------------------------------------------------
  subroutine prismByOne (nZ)
*/
void priIntoPyrAndTet(Subdivision *this, int *nodeIdList) {

  int n1=nodeIdList[0],n2=nodeIdList[1],n3=nodeIdList[2],n4=nodeIdList[3],n5=nodeIdList[4],n6=nodeIdList[5];
  Position p1,p2,p3,p4,p5,p6;
  p1 = GetPositionDummy(n1);
  p2 = GetPositionDummy(n2);
  p3 = GetPositionDummy(n3);
  p4 = GetPositionDummy(n4);
  p5 = GetPositionDummy(n5);
  p6 = GetPositionDummy(n6);


/*c----------------------------------------------- calculate the centroid
      xn(nc) = 0.166667 * (xn(n1)+xn(n2)+xn(n3)+xn(n4)+xn(n5)+xn(n6))
      yn(nc) = 0.166667 * (yn(n1)+yn(n2)+yn(n3)+yn(n4)+yn(n5)+yn(n6))
      zn(nc) = 0.166667 * (zn(n1)+zn(n2)+zn(n3)+zn(n4)+zn(n5)+zn(n6))
*/
  Position centroid;
  int newNode = this->subNodesStart+0;
  /*Add up all the positions, then scale by 1/6th */
  centroid = 
    PositionScale(PositionAdd(PositionAdd(PositionAdd(PositionAdd(PositionAdd(
									      p1, p2), p3), p4), p5), p6), 1./6.);
  /*
c----------------------------------------- set boundary marker to "off"
      mBndN(nc) = 0


  */
  /*
c--------------------------------- store the new tetrahedra zone lists,
c				   using faces 1 & 5 as bases,
c				   remembering to zero 5,6,7,8
  */
  Zone newZone;

  ZoneInit(&newZone, n1, n2, n3, newNode, 0, 0, 0, 0);
  SetZoneDummy(this->subZonesStart + 0, newZone);

  ZoneInit(&newZone, n4, n6, n5, newNode, 0, 0, 0, 0);
  SetZoneDummy(this->subZonesStart + 1, newZone);

  /*
      zNodeList(1,nz1) = n1
      zNodeList(2,nz1) = n2
      zNodeList(3,nz1) = n3
      zNodeList(4,nz1) = nc
      zNodeList(5,nz1) = 0
      zNodeList(6,nz1) = 0
      zNodeList(7,nz1) = 0
      zNodeList(8,nz1) = 0

      zNodeList(1,nz2) = n4
      zNodeList(2,nz2) = n6
      zNodeList(3,nz2) = n5
      zNodeList(4,nz2) = nc
      zNodeList(5,nz2) = 0
      zNodeList(6,nz2) = 0
      zNodeList(7,nz2) = 0
      zNodeList(8,nz2) = 0
  */
  /*
c------------------------------------ store the new pyramid zone lists,
c				      using faces 2, 3 & 4 as bases,
c				      remembering to zero 6,7,8
  */
  ZoneInit(&newZone, n1, n3, n6, n4, newNode, 0, 0, 0);
  SetZoneDummy(this->subZonesStart + 2, newZone);

  ZoneInit(&newZone, n1, n4, n5, n2, newNode, 0, 0, 0);
  SetZoneDummy(this->subZonesStart + 3, newZone);

  ZoneInit(&newZone, n2, n5, n6, n3, newNode, 0, 0, 0);
  SetZoneDummy(this->subZonesStart + 4, newZone);

  /*
      zNodeList(1,nz3) = n1
      zNodeList(2,nz3) = n3
      zNodeList(3,nz3) = n6
      zNodeList(4,nz3) = n4
      zNodeList(5,nz3) = nc
      zNodeList(6,nz3) = 0
      zNodeList(7,nz3) = 0
      zNodeList(8,nz3) = 0

      zNodeList(1,nz4) = n1
      zNodeList(2,nz4) = n4
      zNodeList(3,nz4) = n5
      zNodeList(4,nz4) = n2
      zNodeList(5,nz4) = nc
      zNodeList(6,nz4) = 0
      zNodeList(7,nz4) = 0
      zNodeList(8,nz4) = 0

      zNodeList(1,nz5) = n2
      zNodeList(2,nz5) = n5
      zNodeList(3,nz5) = n6
      zNodeList(4,nz5) = n3
      zNodeList(5,nz5) = nc
      zNodeList(6,nz5) = 0
      zNodeList(7,nz5) = 0
      zNodeList(8,nz5) = 0
*/
}





/*! The triangular prism zone is divided by two new nodes into two
c tetrahedra and three prisms.
c----------------------------------------------------------------------
      subroutine prismByTwo (nZ)
*/
void priIntoPriAndTet(Subdivision *this, int *nodeIdList) {
  int n1=nodeIdList[0],n2=nodeIdList[1],n3=nodeIdList[2],n4=nodeIdList[3],n5=nodeIdList[4],n6=nodeIdList[5];

  Position p1,p2,p3,p4,p5,p6;
  p1 = GetPositionDummy(n1);
  p2 = GetPositionDummy(n2);
  p3 = GetPositionDummy(n3);
  p4 = GetPositionDummy(n4);
  p5 = GetPositionDummy(n5);
  p6 = GetPositionDummy(n6);

 
  /*------------------------------------------------------ face 1 centroid
    xc1 = 0.33 * (xn(n1)+xn(n2)+xn(n3))
    yc1 = 0.33 * (yn(n1)+yn(n2)+yn(n3))
    zc1 = 0.33 * (zn(n1)+zn(n2)+zn(n3))
  */
  Position centroid1, centroid2;
  
  centroid1 = PositionScale(PositionAdd(PositionAdd(p1, p2), p3), 0.33);
  
  /*c------------------------------------------------------ face 2 centroid
    xc6 = 0.33 * (xn(4)+xn(n5)+xn(n6))
    yc6 = 0.33 * (yn(4)+yn(n5)+yn(n6))
    zc6 = 0.33 * (zn(4)+zn(n5)+zn(n6))
  */
  centroid2 = PositionScale(PositionAdd(PositionAdd(p4, p5), p6), 0.33);

  int newNode1 = this->subNodesStart+0;
  int newNode2 = this->subNodesStart+1;
  
  /*c------------------------------------ store the first new node position
      xn(nc1) = xc1 + 0.33 * (xc6 - xc1)
      yn(nc1) = yc1 + 0.33 * (yc6 - yc1)
      zn(nc1) = zc1 + 0.33 * (zc6 - zc1)
  */

  Position newPosition1 = PositionAdd(centroid1, 
				      PositionScale(PositionSubtract(centroid2, centroid1),0.33));
  
  SetPositionDummy(newNode1, newPosition1);


  /*c----------------------------------- store the second new node position
      xn(nc2) = xc1 + 0.67 * (xc6 - xc1)
      yn(nc2) = yc1 + 0.67 * (yc6 - yc1)
      zn(nc2) = zc1 + 0.67 * (zc6 - zc1)
  */
  Position newPosition2 = PositionAdd(centroid1, 
			     PositionScale(PositionSubtract(centroid2, centroid1),0.67));
  
  SetPositionDummy(newNode2, newPosition2);

  /*c---------------------------------------- set boundary markers to "off"
    mBndN(nc1) = 0
    mBndN(nc2) = 0
  */

  /*c---------------------------------- store the new tetrahedra zone lists
c				    with bases on faces 1 & 5,
c				    remembering to zero 5,6,7,8
  */
  Zone newZone;

  ZoneInit(&newZone, n1, n2, n3, newNode1, 0, 0, 0, 0);
  SetZoneDummy(this->subZonesStart + 0, newZone);

  ZoneInit(&newZone, n4, n6, n5, newNode2, 0, 0, 0, 0);
  SetZoneDummy(this->subZonesStart + 1, newZone);

  /*
      zNodeList(1,nz1) = n1
      zNodeList(2,nz1) = n2
      zNodeList(3,nz1) = n3
      zNodeList(4,nz1) = nc1
      zNodeList(5,nz1) = 0
      zNodeList(6,nz1) = 0
      zNodeList(7,nz1) = 0
      zNodeList(8,nz1) = 0

      zNodeList(1,nz2) = n4
      zNodeList(2,nz2) = n6
      zNodeList(3,nz2) = n5
      zNodeList(4,nz2) = nc2
      zNodeList(5,nz2) = 0
      zNodeList(6,nz2) = 0
      zNodeList(7,nz2) = 0
      zNodeList(8,nz2) = 0

c--------------------------------------- store the new prism zone lists
c				         with bases on faces 2,3,& 4,
c				         remembering to zero 7,8
  */
  ZoneInit(&newZone, n1, n2, newNode1, n4, n5, newNode2, 0, 0);
  SetZoneDummy(this->subZonesStart + 2, newZone);

  ZoneInit(&newZone, n2, n3, newNode1, n5, n6, newNode2, 0, 0);
  SetZoneDummy(this->subZonesStart + 3, newZone);

  ZoneInit(&newZone, n3, n1, newNode1, n6, n4, newNode2, 0, 0);
  SetZoneDummy(this->subZonesStart + 4, newZone);


  /*
      zNodeList(1,nz3) = n1
      zNodeList(2,nz3) = n2
      zNodeList(3,nz3) = nc1
      zNodeList(4,nz3) = n4
      zNodeList(5,nz3) = n5
      zNodeList(6,nz3) = nc2
      zNodeList(7,nz3) = 0
      zNodeList(8,nz3) = 0

      zNodeList(1,nz4) = n2
      zNodeList(2,nz4) = n3
      zNodeList(3,nz4) = nc1
      zNodeList(4,nz4) = n5
      zNodeList(5,nz4) = n6
      zNodeList(6,nz4) = nc2
      zNodeList(7,nz4) = 0
      zNodeList(8,nz4) = 0

      zNodeList(1,nz5) = n3
      zNodeList(2,nz5) = n1
      zNodeList(3,nz5) = nc1
      zNodeList(4,nz5) = n6
      zNodeList(5,nz5) = n4
      zNodeList(6,nz5) = nc2
      zNodeList(7,nz5) = 0
      zNodeList(8,nz5) = 0
*/
}




/*!The pyramid zone is divided by one new node into one new pyramid
c and four tetrahedra.
c----------------------------------------------------------------------
      subroutine pyramidByOne (nZ)
*/
void pyrIntoPyrAndTet(Subdivision *this, int *nodeIdList) {

  int n1=nodeIdList[0],n2=nodeIdList[1],n3=nodeIdList[2],n4=nodeIdList[3],n5=nodeIdList[4];
  Position p1,p2,p3,p4,p5;
  p1 = GetPositionDummy(n1);
  p2 = GetPositionDummy(n2);
  p3 = GetPositionDummy(n3);
  p4 = GetPositionDummy(n4);
  p5 = GetPositionDummy(n5);


  /*-------------------------------------------------------- base centroid
      xf1 = 0.25 * (xn(n1)+xn(n2)+xn(n3)+xn(n4))
      yf1 = 0.25 * (yn(n1)+yn(n2)+yn(n3)+yn(n4))
      zf1 = 0.25 * (zn(n1)+zn(n2)+zn(n3)+zn(n4))
  */
  Position centroid;
  int newNode = this->subNodesStart+0;
  /*Add up all the positions, then scale by 1/6th */
  centroid = 
    PositionScale(PositionAdd(PositionAdd(PositionAdd(
									      p1, p2), p3), p4), 1./4.);

  /*------------------------------------------ store the new node position
      xn(nc) = xf1 + 0.30 * (xn(n5) - xf1)
      yn(nc) = yf1 + 0.30 * (yn(n5) - yf1)
      zn(nc) = zf1 + 0.30 * (zn(n5) - zf1)
  */
  Position newPosition;

  newPosition = PositionAdd(centroid, 
			     PositionScale(PositionSubtract(p5, centroid),0.30)); 

  SetPositionDummy(newNode, newPosition);


  /*c----------------------------------------- set boundary marker to "off"
    mBndN(nc) = 0
  */

  /*c-------------------------------------- store the new pyramid zone list
    c				        with base on face 1,
    c				        remembering to zero 6,7,8
  */

  Zone newZone;

  ZoneInit(&newZone, n1, n2, n3, n4, newNode, 0, 0, 0);
  SetZoneDummy(this->subZonesStart + 0, newZone);
  /*
      zNodeList(1,nz1) = n1
      zNodeList(2,nz1) = n2
      zNodeList(3,nz1) = n3
      zNodeList(4,nz1) = n4
      zNodeList(5,nz1) = nc
      zNodeList(6,nz1) = 0
      zNodeList(7,nz1) = 0
      zNodeList(8,nz1) = 0
  */
  /*c---------------------------------- store the new tetrahedra zone lists
    c				    with bases on faces 2,3,4 & 5,
    c				    remembering to zero 5,6,7,8
  */
  ZoneInit(&newZone, n1, n5, n2, newNode, 0, 0, 0, 0);
  SetZoneDummy(this->subZonesStart + 1, newZone);

  ZoneInit(&newZone, n2, n5, n3, newNode, 0, 0, 0, 0);
  SetZoneDummy(this->subZonesStart + 2, newZone);

  ZoneInit(&newZone, n4, n5, n4, newNode, 0, 0, 0, 0);
  SetZoneDummy(this->subZonesStart + 3, newZone);

  ZoneInit(&newZone, n4, n5, n1, newNode, 0, 0, 0, 0);
  SetZoneDummy(this->subZonesStart + 4, newZone);
  /*
      zNodeList(1,nz2) = n1
      zNodeList(2,nz2) = n5
      zNodeList(3,nz2) = n2
      zNodeList(4,nz2) = nc
      zNodeList(5,nz2) = 0
      zNodeList(6,nz2) = 0
      zNodeList(7,nz2) = 0
      zNodeList(8,nz2) = 0

      zNodeList(1,nz3) = n2
      zNodeList(2,nz3) = n5
      zNodeList(3,nz3) = n3
      zNodeList(4,nz3) = nc
      zNodeList(5,nz3) = 0
      zNodeList(6,nz3) = 0
      zNodeList(7,nz3) = 0
      zNodeList(8,nz3) = 0

      zNodeList(1,nz4) = n3
      zNodeList(2,nz4) = n5
      zNodeList(3,nz4) = n4
      zNodeList(4,nz4) = nc
      zNodeList(5,nz4) = 0
      zNodeList(6,nz4) = 0
      zNodeList(7,nz4) = 0
      zNodeList(8,nz4) = 0

      zNodeList(1,nz5) = n4
      zNodeList(2,nz5) = n5
      zNodeList(3,nz5) = n1
      zNodeList(4,nz5) = nc
      zNodeList(5,nz5) = 0
      zNodeList(6,nz5) = 0
      zNodeList(7,nz5) = 0
      zNodeList(8,nz5) = 0
  */
}

void hexIntoHexAndPri(Subdivision *this, int *nodeIdList) {
  fprintf(stderr, "Not implemented");
  assert(0);
}
void hexIntoHex(Subdivision *this, int *nodeIdList){
  fprintf(stderr, "Not implemented");
  assert(0);
}
void priIntoPri(Subdivision *this, int *nodeIdList){
  fprintf(stderr, "Not implemented");
  assert(0);
}
void pyrIntoPriAndPyrAndTet(Subdivision *this, int *nodeIdList){
  fprintf(stderr, "Not implemented");
  assert(0);
}
void pyrIntoHexAndPyr(Subdivision *this, int *nodeIdList){
  fprintf(stderr, "Not implemented");
  assert(0);
}
