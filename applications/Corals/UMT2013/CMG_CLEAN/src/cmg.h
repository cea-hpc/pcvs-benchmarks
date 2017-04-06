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
  // Benjamin T. Grover, Fri Oct 21 13:26:58 PDT 2005
  //
  
  All function calls are preceded by a c
*/

/*!\mainpage

The Compact Mesh Generator (CMG) is a simple domain decomposed mesh
generation tool.  Each logical block of the mesh is created on a
seperate domain.  One can also make exact copies of the whole mesh on
multiple domains.

For most uses linking in libcmg.a or libcmgp.a and including cmg.h in
your files will be sufficient for using the cmg. See cmg.h for documentation
of mesh generation and query functions.  Most other utilities are generally for internal
use only.

There is a test directory that is also available with this source package which
has some example uses of the cmg.

testApp.c shows simple generation and query of the mesh, along with querying of
send and recieve data.  siloWriter.cc will output a silo mesh that can be read
into VisIt.

To make the libraries, type make cmg, or make cmgp (for parallel).  To make the test
applications type make test, or make testp, and make siloWriter or make siloWriterp.
To make the parallel applications you must be on a machine running mpi.

 // Copyright 2005 The Regents of the University of California.
  // All rights reserved.
 
  //
  // This work was produced at the University of California, Lawrence
  // Livermore National Laboratory (UC LLNL) under contract no.
  // W-7405-ENG-48 (Contract 48) between the U.S. Department of Energy
  // (DOE) and the Regents of the University of California (University)
  // for the operation of UC LLNL.  The rights of the Federal Government are
  // reserved under Contract 48 subject to the restrictions agreed upon by
  // the DOE and University as allowed under DOE Acquisition Letter 97-1.
  //

Authors:  Benjamin T. Grover (grover5@llnl.gov) and Walter Nissen (nissen5@llnl.gov)

*/

/*!\file cmg.h
  \brief Header file to include where access to the cmg is needed.

  FYI:  Get local to global functions declared in this file, have not
  yet been implemented.  Every id in the cmg is a global id.
 
*/


#ifndef CMG_H
#define CMG_H

#include <stdio.h>

/************************************************************
   Begin functions defined in CMGIO.c
************************************************************/


/*! 
   Set the file to read the input from (default is stdin)

   Generally an application will take care of opening the file
   for read, and then pass that file to this function, it is called
   in tandem with creadinp( ) so that the input is actually read.
   
*/
void csetinp(FILE* inputFile);

/* void csetinp_(FILE* inputFile) There is no Fortran equivalent here. 
In general it is a bad idea to mix FORTRAN and C IO. */

/*! 
   Reads the mesh input from the file that was set in csetinp( ) 
*/
void creadinp ( );

/*! Fortran creadinp(), which still reads from stdin. */
void creadinp_( );

/*! 
  Print the result of parsing the input. 
*/
void cprntinp ( );
/*! Fortran cprntinp() */
void cprntinp_( );

/************************************************************
   End functions defined in CMGIO.c
************************************************************/

/************************************************************
   Begin functions defined in CMGGenerator.c
************************************************************/

/*!
  Generates the mesh on this domain.

  Should be called only after all the mesh is read in to the domains.
  Topology is generated with this command and mesh is setup. Truthfully,
  when this is called the mesh is not actually generated.  The mesh is
  only generated when it is queried.  
*/
void cgenmesh ( );
/*! Fortran cgenmesh() */
void cgenmesh_( );

/*!
  Broadcast the input that is parsed by the deck. On a serial run this is a no op.

  If you are running in parallel this generally must be called before you call cgenmesh,
  otherwise the input data will not be broadcast.

  So in general you will want to read in the data, broadcast the input then,
  call cgenmesh.  Then the mesh is ready to query.
*/
void bcastinp( );
/*! Fortran bcastinp( ) */
void bcastinp_( );



/************************************************************
   End functions defined in CMGGenerator.c
************************************************************/

/************************************************************
   Begin functions defined in CMGDomainQuery.c
************************************************************/

/*!
  Get the size of the local to global faces array
*/
void cltgfsz ( int *size );
/*! Fortran cltgfsz() */
void cltgfsz_( int *size );

/*!
  Get the local to global faces array
*/
void cltgf ( int *faces );
/*! Fortran cltgf() */
void cltgf_( int *faces );

/*!
  Get the size of the local to global nodes array
*/
void cltgnsz ( int *size );
/*! Fortran cltgnsz() */
void cltgnsz_( int *size );

/*!
  Get the local to global nodes array
*/
void cltgn (int *nodes );
/*! Fortran cltgn() */
void cltgn_(int *nodes );

/*!
  Get the size of the local to global zones array
*/
void cltgzsz ( int *size );
/*! Fortran cltgzsz() */
void cltgzsz_( int *size );

/*!
  Get the local to global zones array
*/
void cltgz ( int *zones );
/*! Fortran cltgz() */
void cltgz_( int *zones );

/*!
  Get the size of the local to global edges array
*/
void cltgesz ( int *size );
/*! Fortran cltgesz() */
void cltgesz_( int *size );

/*!
  Get the local to global edges array
*/
void cltge ( int *edges );
/*! Fortran cltge() */
void cltge_( int *edges );

/*!
  Get the size of the recieve faces array
  
  This is the size of the domain array and size of the domain
  face list array for example:
  [domid1, domid2, domid3] and [numfaces1, numfaces2, numfaces3]
*/ 
void crecfsz ( int *size );

/*! Fortran crecfsz() */
void crecfsz_( int *size );

/*!
Get the domain id array and the faces per domain array

Returns two arrays, that need to be allocated then passed in.
The first is an array of domain ids, the second is the number of
faces corresponding to that array.
*/
void crecfar (int *domar, int *fcszar );
/*! Fortran crecfar() */
void crecfar_(int *domar, int *fcszar );

/*!
  Get the recieve faces for the given domain
*/
void crecf ( int domid, int *faces );
/*! Fortran crecf() */
void crecf_( int *domid, int *faces );

/*!
  Get the size of the recieve nodes array
*/
void crecnsz ( int *size );
/*! Fortran crecnsz() */
void crecnsz_( int *size );

/*!
  Get the domain id array and the nodes per domain array
*/
void crecnar ( int *domar, int *nszar );
/*! Fortran crecnar() */
void crecnar_( int *domar, int *nszar );

/*!
  Get the receive nodes for the given domain
*/
void crecn ( int domid, int *nodes );
/*! Fortran crecn() */
void crecn_( int *domid, int *nodes );

/*!
  Get size of the receive zones array
*/
void creczsz ( int *size );
/*! Fortran creczsz() */
void creczsz_( int *size );

/*!
  Get the domain id array and the zones per domain array
*/
void creczar ( int *domar, int *zszar );
/*! Fortran creczar() */
void creczar_( int *domar, int *zszar );

/*!
  Get the recieve zones for a given domain
*/
void crecz ( int domid, int *zones );
/*! Fortran crecz() */
void crecz_( int *domid, int *zones );

/*!
  Get the size of the send faces array
*/
void csndfsz (int *size );
/*! Fortran csndfsz() */
void csndfsz_(int *size );

/*!
  Get the domain id array and the faces per domain array
*/
void csndfar ( int *domar, int *fszar );
/*! Fortran csndfar() */
void csndfar_( int *domar, int *fszar );

/*!
  Get the send faces for the given domain
*/
void csndf (int domid, int *faces);
/*! Fortran csndf() */
void csndf_(int *domid, int *faces);

/*!
  Get the size of the send nodes array
*/
void csndnsz ( int *size );
/*! Fortran csndnsz() */
void csndnsz_( int *size );

/*!
  Get the domain id array and the nodes per domain array
*/
void csndnar (int *domar, int *nszar);
/*! Fortran csndnar() */
void csndnar_(int *domar, int *nszar);

/*!
  Get the send nodes for the given domain
*/
void csndn ( int domid, int *nodes );
/*! Fortran csndn() */
void csndn_( int *domid, int *nodes );

/*!
  Get the size of the send zones array
*/
void csndzsz ( int *size );
/*! Fortran csndzsz() */
void csndzsz_( int *size );

/*!
  Get the domain id array and the zones per domain array
*/
void csndzar (int *domar, int *zszar );
/*! Fortran csndzar() */
void csndzar_(int *domar, int *zszar );

/*!
  Get the send zones for the given array
*/
void csndz ( int domid, int *zones );
/*! Fortran csndz() */
void csndz_( int *domid, int *zones );

/************************************************************
   End functions defined in CMGDomainQuery.c
************************************************************/

/************************************************************
   Begin functions defined in CMGTagQuery.c
************************************************************/

/*!
  Get the number of mesh tags
*/
void cmtgs ( int *numtags );
/*! Fortran cmtgs() */
void cmtgs_( int *numtags );

typedef enum {CMG_NODE=0, CMG_EDGE=1, CMG_FACE=2, CMG_ZONE=3, CMG_MATERIAL=4} cMeshTagType;

/*! 
  Get the number of mesh tags with the given mesh tag type
*/
void cmtgstp ( cMeshTagType tagtyp, int *numtags );
/*! Fortran cmtgstp() */
void cmtgstp_( int *tagtyp, int *numtags );

/*!
  Get the name of the tag at the given mesh tag index
*/
void cmtgnm (int tagindx, char *tagnm );
/*! Fortran cmtgnm() */
void cmtgnm_(int *tagindx, char *tagnm, int *tagnmlen, long dummylen );

/*!
  Get the mesh tag type for the given mesh tag index 
  Tag types are 0-node, 1-edge, 2-face, 3-zone, 4-material
*/
void cmtgtp (int tagindx, int *tagtyp );
/*! Fortran cmtgtp() */
void cmtgtp_(int *tagindx, int *tagtyp );

/*! 
  Get the number of ids tagged by the given mesh tag index
*/
void cmtgidsz (int tagindx, int *idssz);
/*! Fortran version of cmtgidsz() */
void cmtgidsz_(int *tagindx, int *idssz);

/*!
  Get the ids of the objects for the given mesh tag index
*/
void cmtgid (int tagindx, int *ids);
/*! Fortran cmtgid() */
void cmtgid_(int *tagindx, int *ids );

/*!
  Get number of tags associated with a given zone
*/
void cmtgfzsz (int zid, int *tagsz);
/*! Fortran cmtgfzsz( ) */
void cmtgfzsz_(int *zid, int *tagsz);

/*!
  Get the tagids associated with the given zone
*/
void cmtgfz(int zid, int *tagids);
/* Fortran cmtgfz( ) */
void cmtgfz_(int *zid, int *tagids);





/************************************************************
   End functions defined in CMGTagQuery.c
************************************************************/

/************************************************************
   Begin functions defined in CMGMeshQuery.c
************************************************************/

/*!
  Get the number of nodes on this domain
*/
void cnumnod (int *nnodes);
/*! Fortran cnumnod() */
void cnumnod_(int *nnodes);



/*!
  Get the nodes on this domain
 */
void cnods(int *nodes);
/*!
  Fortran cnods()
*/
void cnods_(int *nodes);

/*!
  Get the node position for the given node
*/
void cnodpos (int nodeid, double *x, double *y, double *z);
/*! Fortran cnodpos() */
void cnodpos_(int *nodeid, double *x, double *y, double *z);

/*!
  Get all node positions for the domain
*/
void cndspos(double *x, double *y, double *z);
/*! Fortran cnodpos() */
void cndspos_(double *x, double *y, double *z);


/*!
  Get the number of faces on this domain
*/
void cnumfcs (int *nfaces);
/*! Fortran cnumfcs() */
void cnumfcs_(int *nfaces);








/*!
  Get the number of zones on this domain
*/
void cnumzns (int *nzones);
/*! Fortran cnumzns() */
void cnumzns_(int *nzones);

/*!
  Get the the zones on this domain
*/
void czns(int *zones);
/*! Fortran czns( ) */
void czns_(int *zones);


typedef enum {CMG_INVALID=0, CMG_TET=4, CMG_PYR=5, CMG_PRI=6, CMG_HEX=8} cZoneType;

/*!
  Get the zone type for a given zone
  8-hex, 6-prism, 5-pyramid, 4-tetrahedron
*/
void cgetztp (int gloid, cZoneType *typ);
/*! Fortran cgetztp() */
void cgetztp_(int *gloid, int *typ);

/*!
  Get the nodes in order for the given zone
*/
void cgetznn (int gloid, int *nodes);
/*! Fortran cgetznn() */
void cgetznn_(int *gloid, int *nodes);

/************************************************************
   End functions defined in CMGMeshQuery.c
************************************************************/

/************************************************************
   Begin functions defined in CMGGlobalMeshQuery.c
************************************************************/
/* sms query
 */
/*! Get the sms index for a given task
 */
void cgetsms(int *i, int *j, int *k);

/*!
  Fortran cgetsms
*/
void cgetsms_(int *i, int *j, int *k);

/*Boundary Face query functions
 */
/*!Get the number of boundary faces on this domain, whether owned or not
 */
void cnumbdf(int *nfaces);

/*!
  Fortran cbdrfcs( ) */
void cnumbdf_(int *nfaces);

/*!Get the list of boundary face ids on this domain, whether owned or not
 */
void cbdrfc(int *faces);

/* Fortran cbdrfc( )*/
void cbdrfc_(int *faces);

/*!Get the four nodes for the given boundary face, no matter what domain
 */
void cfcnda(int facid, int *nodid);

/*!Fortran cfcnda( )*/
void cfcnda_(int *facid, int *nodid);



/* Node query functions
 */
/*!
  This returns a count of ALL the nodes that are in or touch the domain
  boundary
*/
void cnmnoda (int *nnodes);

/*!Fortran cnmnoda( )*/
void cnmnoda_(int *nnodes);

/*!Used in conjunction with cnmnoda,
  returns all node ids that touch or are in the domain,
  giving the offset of where in the array the non-owned
  nodes start
*/
void cnodsa(int *nodes, int *offset);

/*!
  Fortran cnodsa( )
*/
void cnodsa_(int *nodes, int *offset);

/************************************************************
  End functions defined in CMGGlobalMeshQuery.c
************************************************************/

/************************************************************
   Begin functions defined in meshAndInputData.c
************************************************************/
/*!
  Cleanup all the data associated with the mesh
*/
void cclnmd( );
/*!Fortran cclnmd() */
void cclnmd_( );


/************************************************************
   End functions defined in meshAndInputData.c
************************************************************/



/************************************************************
 SERIAL FUNCTIONS THAT ASK FOR A DOMAIN ID FOR DOMAIN QUERY
************************************************************/

/*!
  SERIAL
  Get the size of the recieve faces array
  This is the size of the domain array and size of the domain
  face list array for example:
  [domid1, domid2, domid3] and [numfaces1, numfaces2, numfaces3].
*/ 
void crecfszT ( int *size , int domainId);
/*!
  SERIAL
Get the domain id array and the faces per domain array
*/
void crecfarT (int *domar, int *fcszar , int domainId);

/*!
  SERIAL
  Get the faces for each of my recive domains
 */
void crecfT ( int thisDomain, int domid, int *faces );

/*!
  SERIAL
  Get size of the receive zones array
*/
void creczszT ( int *size ,int domainId);
/*!
  SERIAL
  Get the domain id array and the zones per domain array
*/
void creczarT ( int *domar, int *zszar ,int domainId );

void creczT(int thisDomain, int domid, int *zones);


/*!
  SERIAL
  Get the size of the send faces array
*/
void csndfszT (int *size , int domainId);

/*!
  SERIAL
  Get the domain id array and the faces per domain array
*/
void csndfarT ( int *domar, int *fszar, int domainId );

/*Get the actual send faces for this domain to its given
  send domain
*/
void csndfT(int thisDomain, int domid, int *faces);

/*!
  SERIAL
  Get the domain id array and the nodes per domain array
*/
void csndnarT (int *domar, int *nszar, int domainId);
/*!
  SERIAL
  Get the size of the send zones array
*/
void csndzszT ( int *size , int domainId);

/*!
  SERIAL
  Get the domain id array and the zones per domain array
*/
void csndzarT (int *domar, int *zszar , int domainId);

void csndzT(int thisDomain, int domid, int *zones);


/*!
  SERIAL
  Get the size of the send nodes array
*/
void csndnszT ( int *size , int domainId);

void csndnarT( int *domar, int *nszar, int domainId);

void csndnT(int thisDoman, int domainId, int *nodes);


/*!
  SERIAL
  Get the size of the recieve nodes array
*/
void crecnszT ( int *size , int domainId);

/*!
  SERIAL
  Get the domain id array and the nodes per domain array
*/
void crecnarT ( int *domar, int *nszar , int domainId);

void crecnT(int thisDomain, int domid, int *nodes);



/************************************************************
END SERIAL FUNCTIONS THAT ASK FOR A DOMAIN ID FOR DOMAIN QUERY
************************************************************/


#endif /*  CMG_H */
