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
 * History:
 *  1/15/96    gt    Added additional data types.
 *  2/15/96    gt    Added MPITEST_comm_type_self, _merge.
 *  2/20/96    gt    Added derived data types.
 *
 */

/* Optional data types.  Set to 1 if implementation under test supports
   long long int and/or long double */
#define MPITEST_longlong_def 1
#define MPITEST_long_double_def 1

/* C data type that corresponds to MPI_byte */
#define MPITEST_byte_def unsigned char

/* The default length of specification arrays.  This number should be
   set large enough to accommodate all foreseeable default specs. */
#define MPITEST_CONFIG_ARRAY_SIZE 128

/* The global end token.  This delimits the end of the 
   specification arrays.  */
#define MPITEST_END_TOKEN -999999




/* Magic numbers for message length specification */ 
#define MPITEST_ADD_INC -1
#define MPITEST_MULT_INC -2
#define MPITEST_REPEAT -3
#define MPITEST_MIN_LENGTH -3


/* communicator size tokens */
#define MPITEST_comm_one -1
#define MPITEST_comm_half_of_all -2
#define MPITEST_comm_all_but_one -3
#define MPITEST_comm_all -4
#define MPITEST_comm_last_rank -5
#define MPITEST_comm_inc -6
#define MPITEST_node_list -7
#define MPITEST_comm_size_min -7


/* communicator type tokens */
#define MPITEST_comm_type_world -10
#define MPITEST_comm_type_self -11
#define MPITEST_comm_type_create -12
#define MPITEST_comm_type_split -13
#define MPITEST_comm_type_dup -14
#define MPITEST_comm_type_inter -15
#define MPITEST_comm_type_merge -16
#define MPITEST_comm_type_min -17


/* These defines are indices into the MPITEST_mpi_datatypes[] array defined
   in mpitest_user.h.  Append entries here and initialize in mpitest_user.h,
   but do not modify these or you may cause unnecessary failures.  */
#define MPITEST_int 0
#define MPITEST_short_int 1
#define MPITEST_long 2
#define MPITEST_unsigned_short 3
#define MPITEST_unsigned 4
#define MPITEST_unsigned_long 5
#define MPITEST_float 6
#define MPITEST_double 7
#define MPITEST_char 8
#define MPITEST_unsigned_char 9
#define MPITEST_longlong 10
#define MPITEST_long_double 11
#define MPITEST_byte 12
#define MPITEST_derived1 13
#define MPITEST_derived2 14
#define MPITEST_datatype_max 14


/* This magic number is used to indicate that the corresponding parameter
   has been given on the command line */
#define MPITEST_COMMAND_LINE -999

/* This definition is used to create a global "not assigned" value
   which can be assigned to command-line parameters.  This value
   is assigned if there is no specification of the given parameter on
   the command line. */
#define MPITEST_NOT_ASSIGNED -111

/* Max ranks */
#define MPITEST_MAX_RANKS  64

/*  For future enhancement  */
#define MPITEST_COMM_WORLD MPI_COMM_WORLD

/* Structure used for data assignments.  This structure should contain
   a member for each data type which may be used by a test. */

struct dataTemplate
{
  int            Int;
  short int      ShortInt;
  long int       Long;
  unsigned short UnsignedShort;
  unsigned       Unsigned;
  unsigned long  UnsignedLong;
  float          Float;
  double         Double;
  signed char    Char;
  unsigned char  UnsignedChar;
#if MPITEST_longlong_def
  long long int  LongLong;
#endif
#if MPITEST_long_double_def
  long double    LongDouble;
#endif
  MPITEST_byte_def Byte;
};

/* Structures used for derived data types. */

typedef struct derived1
{
  int            Int[2];
  short int      ShortInt[2];
  long int       Long[2];
  unsigned short UnsignedShort[2];
  unsigned       Unsigned[2];
  unsigned long  UnsignedLong[2];
  float          Float[2];
  signed char    Char[2];
  double         Double[2];
  unsigned char  UnsignedChar[2];
#if MPITEST_longlong_def
  long long int  LongLong[2];
#endif
#if MPITEST_long_double_def
  long double    LongDouble[2];
#endif
} derived1;
