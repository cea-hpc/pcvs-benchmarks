/*
  
// Copyright 2006 The Regents of the University of California.
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
 // Benjamin T. Grover, Thu Feb 09 16:43:51 PST 2006
 //
 Utilities that are specifically used in the CMGDomainQuery.
 */
#ifndef __CMG_DOMAIN_QUERY_H__
#define __CMG_DOMAIN_QUERY_H__

/*!\file
  Header file specific to querying information on a domain.

  These methods should never be called by the application using the
  cmg.  They are for internal use.
*/



/*Since send and Receive are compliments of each other, these fuctions
  can get both types of information, if send ==0 it will get recieve
  information, if send == 1 then they will get send information
*/

void getSendReceiveFacesSize( int *size, int domainId, int send);
void getSendReceiveFacesArray( int *domar, int *fcszar, int domainId, int send);
void getSendReceiveFaces(int thisDomain, int domid, int *faces, int send);

void getSendReceiveZonesSize( int *size, int domainId, int send);
void getSendReceiveZonesArray( int *domar, int *znszar, int domainId, int send);
void getSendReceiveZones(int thisDomain, int domid, int *zones, int send);

void getSendReceiveNodesSize( int *size, int domainId, int send);
void getSendReceiveNodesArray( int *domar, int *ndszar, int domainId, int send);
void getSendReceiveNodes(int thisDomain, int domid, int *nodes, int send);



#endif
