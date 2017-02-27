/*-----------------------------------------------------------------------------
MESSAGE PASSING INTERFACE TEST CASE SUITE

Copyright - 1996 Intel Corporation

Intel Corporation hereby grants a non-exclusive license under Intel's
copyright to copy, modify and distribute this software for any purpose
and without fee, provided that the above copyright notice and the following
paragraphs appear on all copies.

Intel Corporation makes no representation that the test cases comprising
this suite are correct or are an accurate representation of any standard.

IN NO EVENT SHALL INTEL HAVE ANY LIABILITY FOR ANY DIRECT, INDIRECT OR
SPECULATIVE DAMAGES, (INCLUDING WITHOUT LIMITING THE FOREGOING, CONSEQUENTIAL,
INCIDENTAL AND SPECIAL DAMAGES) INCLUDING, BUT NOT LIMITED TO INFRINGEMENT,
LOSS OF USE, BUSINESS INTERRUPTIONS, AND LOSS OF PROFITS, IRRESPECTIVE OF
WHETHER INTEL HAS ADVANCE NOTICE OF THE POSSIBILITY OF ANY SUCH DAMAGES.

INTEL CORPORATION SPECIFICALLY DISCLAIMS ANY WARRANTIES INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
PARTICULAR PURPOSE AND NON-INFRINGEMENT.  THE SOFTWARE PROVIDED HEREUNDER
IS ON AN "AS IS" BASIS AND INTEL CORPORATION HAS NO OBLIGATION TO PROVIDE
MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS OR MODIFICATIONS.
-----------------------------------------------------------------------------*/
/*
 * Global include file for MPI test suite.  
 * 
 */

#include "mpi.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define TRUE  1
#define FALSE 0

#define MPITEST_FILENAME_MAX 256
#define MPITEST_INFOBUF_MAX  512

/* If this is a very old version of MPI, define MPI_VERSION (should be
   part of mpi.h for MPI 1.2 and later) */
#ifndef MPI_VERSION
#define MPI_VERSION 1
#endif

/* Global variables.  These values are set at runtime in the library funtion
   MPITEST_init() */

#ifdef _MPITEST_LIB
int MPITEST_nump;         /* total number of proceses in the application */
int MPITEST_me;           /* the calling processes rank in MPI_COMM_WORLD */
int MPITEST_current_rank; /* the calling proc. rank in the current communicator;
			     this is MPI_UNDEFINED if this proc. is not in the
			     current communicator */
int MPITEST_inter;	  /* indicates if intercommunicator, and if so, which
			     half of the communicator (0, 1 or MPI_UNDEFINED) */
int MPITEST_verbose;      /* indicates level of runtime info desired.*/
FILE *MPITEST_stdout = 0; /* where to write output */

/* user codes need to know one magic number : the comm type 
  corresponding to MPI_COMM_WORLD.  This variable holds it. */
int MPITEST_world;

/* This array may be indexed to pull out an MPI_Datatype
   for use in MPI calls.  The indices are given symbolic
   definitions in config.h .  The index should be obtained
   by a call to MPITEST_get_datatype(i), where i is
   an iteration count between 0 and MPITEST_num_datatypes() 

   Append more if you wish, but never modify this */

MPI_Datatype MPITEST_mpi_datatypes[ ]=
{
0, /* MPI_INT */
0, /* MPI_SHORT */
0, /* MPI_LONG */
0, /* MPI_UNSIGNED_SHORT */
0, /* MPI_UNSIGNED */
0, /* MPI_UNSIGNED_LONG */
0, /* MPI_FLOAT */
0, /* MPI_DOUBLE */
0, /* MPI_CHAR */
0, /* MPI_UNSIGNED_CHAR, optional */
0, /* MPI_LONG_LONG_INT, optional */
0, /* MPI_LONG_DOUBLE */
0, /* MPI_BYTE */
0, /* derived 1 */
0, /* derived 2 */
0,                      /* other user-defined data types */
0,
0,
0,
0,
0
};

#else
extern int MPITEST_nump;
extern int MPITEST_me;
extern int MPITEST_current_rank;
extern int MPITEST_inter;
extern int MPITEST_verbose;
extern FILE *MPITEST_stdout;
extern int MPITEST_world;

extern MPI_Datatype MPITEST_mpi_datatypes[];

#endif

/* global macros */
#define MPITEST_FATAL 9
#define MPITEST_NONFATAL 7
#define MPITEST_VERIFY 10
#define MPITEST_INFO0 8
#define MPITEST_INFO1 5
#define MPITEST_INFO2 6

/* In MPITEST_inter, indicates the created communicator is NOT an
   intercommunicator.  Otherwise, it will be set to 0, 1 or MPI_UNDEFINED  */
#define MPITEST_NOT_INTER -1 

/*
 *  Function declarations 
 *  these are all library functions defined in libmpitest.c
 */

int MPITEST_init( int argc, char *argv[]);
int MPITEST_get_parameters( int argc, char *argv[]);
int MPITEST_help_message( void);
int MPITEST_message( int severity, char *message);
int MPITEST_report(int pass, int fail, int verify, char *testname);

int MPITEST_num_message_lengths( void);
int MPITEST_get_message_length(int );
int MPITEST_get_max_message_length( void );
int MPITEST_byte_to_element( int buf_type, int byte_length);

int MPITEST_dataTemplate_init( struct dataTemplate *, int);

int MPITEST_get_comm_index( int);
int MPITEST_get_comm_size( int type, int index);
int MPITEST_num_comm_sizes( void);
int MPITEST_get_comm_type( int);

int MPITEST_num_datatypes( void);
int MPITEST_get_datatype( int);

int MPITEST_get_communicator(int commtype, int index, MPI_Comm *comm);
int MPITEST_create_communicator( int index, MPI_Comm *comm, int split);
int MPITEST_get_split_communicator( int index, MPI_Comm *comm);
int MPITEST_get_intercommunicator( int index, MPI_Comm *comm);
int MPITEST_get_duped_communicator( int index, MPI_Comm *comm);
int MPITEST_free_communicator( int comm_type, MPI_Comm *comm);


int MPITEST_init_buffer( int buff_type, int length , 
			struct dataTemplate value, void *buffer);
int MPITEST_init_buffer_inc( int buff_type, int length , 
			struct dataTemplate value, void *buffer);
int MPITEST_init_buffer_v(  int buff_type, int numblocks, int *counts, 
			  int *displs,  struct dataTemplate *values, void *buffer);

int MPITEST_buffer_errors(  int buffer_type, int length, struct dataTemplate value, 
			  void *buffer);
int MPITEST_buffer_errors_inc(  int buffer_type, int length, struct dataTemplate value, 
			  void *buffer);
int MPITEST_buffer_errors_ov(  int buffer_type, int length, struct dataTemplate value, 
			  void *buffer);
int MPITEST_buffer_errors_v(  int buffer_type, int numblocks, 
			    int *counts, int *displs, struct dataTemplate *values, 
			    void *buffer);

int MPITEST_get_buffer( int buffer_type, int length, void **buffer);

int MPITEST_datatype_has_sticky_ub(MPI_Datatype type, MPI_Aint *ub_off_p);
int MPITEST_datatype_has_sticky_lb(MPI_Datatype type, MPI_Aint *lb_off_p);

const char *MPITEST_GetErrName( int errClass );
