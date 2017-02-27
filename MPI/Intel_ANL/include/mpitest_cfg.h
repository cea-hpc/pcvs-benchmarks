/*******************************************************************************
 config.h : Private include file for library functions for MPITEST suite.
*******************************************************************************/

#include <math.h>
#include "mpitest_def.h"
#ifdef HAVE_WINDOWS_H
#define sleep(a_) Sleep((a_)*1000)
#include <windows.h>
#endif

#define KILO 1024
#define MEGA 1024*1024

int MPITEST_message_lengths[MPITEST_CONFIG_ARRAY_SIZE] = 
{
  0,
  MPITEST_MULT_INC, 8, 8000, 10, 
  MPITEST_REPEAT, 320, 8, 
  48, 
  MPITEST_ADD_INC, KILO-8, KILO+8, 8,
  65536,
  MPITEST_END_TOKEN
  };

/**********  Communicator size and type default specifications ********

  New magic numbers may be defined, in which case new code must 
  be added to the library source code.  See the User's Guide and 
  the comments in the source code for more details.

*********************************************************************/

/* Default specification array for communicator size.  This array
   must consist of a number of sequences formed from a communicator
   type token followed by one or more communicator size tokens.  
   Communicator size tokens may be either magic numbers from the
   file defs.h or integers less than or equal to the number
   of proceses in the application.  Note that the communicator
   size token MPITEST_comm_inc must be followed by three integers:
   the starting rank, the ending rank, and the rank increment.  The
   ending rank must be a legal rank in the current application. 
   The communicator size token MPITEST_node_list must be followed
   by the number of nodes in the desired communicator, then by a list 
   of (global) ranks which will form the new communicator.  The
   ranks of the processes in the new communicator are determined by the
   order of listing in the MPITEST_comm_sizes[] array.

    Caution - these must correspond to valid communicators.  The
    default configuration requires 3 ranks. */

int MPITEST_comm_sizes[ MPITEST_CONFIG_ARRAY_SIZE] = 
{
/*0*/  MPITEST_comm_type_world,

/*1*/  MPITEST_comm_type_self,

/*2*/  MPITEST_comm_type_merge, 
/*3*/   MPITEST_comm_type_create,
/*4*/    MPITEST_comm_inc, 0, 2, 2, 
/*8*/   MPITEST_comm_type_create,
/*9*/    MPITEST_comm_inc, 1, MPITEST_comm_last_rank, 2, 

/*13*/  MPITEST_comm_type_split, 
/*14*/   MPITEST_comm_inc, 1, MPITEST_comm_last_rank, 2, 

/*18*/  MPITEST_comm_type_split, 
/*19*/   MPITEST_node_list, 2, 0, 2,

/*23*/  MPITEST_comm_type_dup, 
/*24*/   MPITEST_comm_inc, 0, MPITEST_comm_last_rank, 3,

/*28*/  MPITEST_comm_type_inter, 
/*29*/   MPITEST_comm_type_create,
/*30*/    MPITEST_comm_inc, 0, 2, 2, 
/*34*/   MPITEST_comm_type_create,
/*35*/    MPITEST_comm_inc, 1, MPITEST_comm_last_rank, 2, 

/*39*/  MPITEST_END_TOKEN

};

/*********************** DATA TYPES*********************************** 

MPITEST_types[] is set up to act as a user-configurable definition of
which types should be looped over in the default testing environment.

The entries in this array are defined in mpitest_defs.h, and another important 
array, MPITEST_mpi_datatypes[] is also defined there.

**********************************************************************/

int MPITEST_types[] =
{
MPITEST_int, MPITEST_short_int, MPITEST_long, MPITEST_unsigned_short, 
MPITEST_unsigned, MPITEST_unsigned_long, MPITEST_float, MPITEST_double, 
MPITEST_char, MPITEST_unsigned_char, MPITEST_long_double, MPITEST_byte,
MPITEST_derived1,MPITEST_derived2,MPITEST_END_TOKEN
};

/* Some constant and compiler directives */

/* The minimum value for MPI_TAG_UB mandated by the MPI Standard */
#define MPITEST_TAG_UB_MIN  32767

/* Compiler directives:
   a few tests which do not use the library use these values (mostly
   derived datatypes) */

/* Comment the following #define out to turn off receiving buffer */
#define MPITEST_BUFFER_RECV   1

/* Comment the following #define out to turn off recv buffer checking */
/* MPITEST_BUFFER_CHK should not be defined if MPITEST_BUFFER_RECV is */
/* NOT defined */
#define MPITEST_BUFFER_CHK    2

/* Comment out the following MACRO to turn off displaying  */
/* the entire sneder & receiver buffer when error occurs   */
/* (output file could be huge! Test may takes a long time to run */
/* but it is useful for debugging test failures) Off by default. */
/* #define MPITEST_DISP_BUF      3 */

/* Comment the following #define out to turn off verification for  */
/* the status object returned from MPI_Recv()                      */ 
#define MPITEST_STATUS_CHK    4

/* Determine whether all node should be synchronized after each */
/* test is done, comment it out if not desired.  */
#define MPITEST_SYNC          5

/* Comment out the following MACRO if user wish to see all   */
/* errors in received data buffer, otherwise, only the first */
/* error in each transmission will be displayed              */
#define MPITEST_1ST_ERR       6

