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
/*******************************************************************************
*                          Error test for MPI_Allreduce()
* 
* This test verifies that the correct error is returned if MPI_Allreduce is
* called with an invalid argument.
*
* 1)  Invalid type/operator pairs.
* 2)  Use mis-matched types.
* 3)  Use invalid type.
* 4)  Use invalid size (-1).
* 5)  Use MPI_COMM_NULL.
* 6)  Use a freed communicator.
* 7)  Use an inter-communicator.
*
* In all cases, expect to receive appropriate error.
* 
* 
* Test history:
*    1  01/12/96     gt   Created
*       11/04/02    wdg   Changed to allows char/uchar for most ops
*                         and double for logicals.  Old behavior
*                         by setting USE_ONLY_SPECIFIED_REDUCE_OPS
* 
*******************************************************************************/
#include "mpitest_cfg.h"
#include "mpitest.h"

int main( int argc, char *argv[])
{
  int 
    pass,
    fail,              /*  counts total number of failures */
    verify,            /*  counts total number of verify failures */
    ierr,              /*  return value from MPI calls     */
    errorclass,        /*  error class of ierr             */
    color,             /*  used to split a communicator    */
    size;

  double
    *input,          /* input to reduce */
    *output;         /* results of reduce */

  char 
    testname[128],     /*  the name of this test           */
    info_buf[256];     /*  for fprintf                     */
    char error_string[MPI_MAX_ERROR_STRING];
  
  
  MPI_Comm comm;       /*  MPI communicators               */

/*
**  Initialize the MPI environment and test environment.
*/
ierr = MPI_Init(&argc, &argv);
if( ierr!=MPI_SUCCESS)
  {
    sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr); 
    MPITEST_message(MPITEST_FATAL, info_buf);
  }

sprintf( testname, "MPI_Allreduce_err1");
MPITEST_init( argc, argv);
if (MPITEST_me==0)
  {
    sprintf(info_buf, "Starting %s test", testname);
    MPITEST_message(MPITEST_INFO0, info_buf);
  }


pass = 0;
fail = 0;
verify = 0;
input = (double *)malloc((MPITEST_nump+1)*sizeof(double));
output = (double *)malloc((MPITEST_nump+1)*sizeof(double));
if ((input == NULL) || (output == NULL))
  MPITEST_message(MPITEST_FATAL, "Couldn't allocate test arrays");

for (size=0; size <= MPITEST_nump; size++) input[size]=0;



/*
**  Set an errorhandler so we get control back.
*/
ierr = MPI_Errhandler_set(MPI_COMM_WORLD,MPI_ERRORS_RETURN);
if (ierr != MPI_SUCCESS)
  {
	fail++;
	sprintf(info_buf, "MPI_Errorhandler_set returned %d", ierr);
	MPITEST_message(MPITEST_FATAL, info_buf);
  }


/*
**  Invalid operator tests.
*/
#define xxx(ty, op)  \
sprintf(info_buf, "about to call MPI_Allreduce w/ invalid op %d on %d",op,ty); \
MPITEST_message( MPITEST_INFO1, info_buf); \
ierr = 0; \
ierr = MPI_Allreduce(input, output, 1, ty, op, MPI_COMM_WORLD); \
if (ierr == MPI_SUCCESS) { \
	fail++; \
	sprintf(info_buf, "MPI_Allreduce w/ invalid op %d on %d did not FAIL",op,ty); \
	MPITEST_message(MPITEST_NONFATAL, info_buf); \
} else { \
	MPI_Error_class(ierr, &errorclass); \
	if (errorclass != MPI_ERR_OP) { \
		fail++; \
		sprintf(info_buf, "MPI_Allreduce w/ invalid op %d on %d returned %d, expected MPI_ERR_OP", \
			errorclass,op,ty); \
		MPITEST_message(MPITEST_NONFATAL, info_buf); \
		MPI_Error_string(ierr, error_string, &size); \
		MPITEST_message( MPITEST_NONFATAL, info_buf); \
	} else { \
		pass++; \
		sprintf(info_buf, "ierr = %d, errorclass = %d", ierr, \
			 errorclass); \
		MPITEST_message( MPITEST_INFO2, info_buf); \
		MPI_Error_string(ierr, error_string, &size); \
		MPITEST_message( MPITEST_INFO1, info_buf); \
	} \
}
xxx(MPI_CHAR, MPI_OP_NULL)
xxx(MPI_SHORT, MPI_OP_NULL)
xxx(MPI_INT, MPI_OP_NULL)
xxx(MPI_LONG, MPI_OP_NULL)
xxx(MPI_UNSIGNED_CHAR, MPI_OP_NULL)
xxx(MPI_UNSIGNED_SHORT, MPI_OP_NULL)
xxx(MPI_UNSIGNED, MPI_OP_NULL)
xxx(MPI_UNSIGNED_LONG, MPI_OP_NULL)
xxx(MPI_FLOAT, MPI_OP_NULL)
xxx(MPI_DOUBLE, MPI_OP_NULL)
xxx(MPI_LONG_DOUBLE, MPI_OP_NULL)
xxx(MPI_BYTE, MPI_OP_NULL)
xxx(MPI_PACKED, MPI_OP_NULL)

#ifdef USE_ONLY_SPECIFIED_REDUCE_OPS
xxx(MPI_CHAR, MPI_MAX)
xxx(MPI_CHAR, MPI_MAXLOC)
xxx(MPI_CHAR, MPI_MIN)
xxx(MPI_CHAR, MPI_MINLOC)
xxx(MPI_CHAR, MPI_SUM)
xxx(MPI_CHAR, MPI_PROD)
xxx(MPI_CHAR, MPI_LAND)
xxx(MPI_CHAR, MPI_LOR)
xxx(MPI_CHAR, MPI_LXOR)
xxx(MPI_CHAR, MPI_BAND)
xxx(MPI_CHAR, MPI_BOR)
xxx(MPI_CHAR, MPI_BXOR)

xxx(MPI_UNSIGNED_CHAR, MPI_MAX)
xxx(MPI_UNSIGNED_CHAR, MPI_MAXLOC)
xxx(MPI_UNSIGNED_CHAR, MPI_MIN)
xxx(MPI_UNSIGNED_CHAR, MPI_MINLOC)
xxx(MPI_UNSIGNED_CHAR, MPI_SUM)
xxx(MPI_UNSIGNED_CHAR, MPI_PROD)
xxx(MPI_UNSIGNED_CHAR, MPI_LAND)
xxx(MPI_UNSIGNED_CHAR, MPI_LOR)
xxx(MPI_UNSIGNED_CHAR, MPI_LXOR)
xxx(MPI_UNSIGNED_CHAR, MPI_BAND)
xxx(MPI_UNSIGNED_CHAR, MPI_BOR)
xxx(MPI_UNSIGNED_CHAR, MPI_BXOR)
#endif

xxx(MPI_BYTE, MPI_MAX)
xxx(MPI_BYTE, MPI_MAXLOC)
xxx(MPI_BYTE, MPI_MIN)
xxx(MPI_BYTE, MPI_MINLOC)
xxx(MPI_BYTE, MPI_SUM)
xxx(MPI_BYTE, MPI_PROD)
xxx(MPI_BYTE, MPI_LAND)
xxx(MPI_BYTE, MPI_LOR)
xxx(MPI_BYTE, MPI_LXOR)
xxx(MPI_DOUBLE, MPI_MAXLOC)
xxx(MPI_DOUBLE, MPI_MINLOC)
#ifdef USE_ONLY_SPECIFIED_REDUCE_OPS
xxx(MPI_DOUBLE, MPI_LAND)
xxx(MPI_DOUBLE, MPI_LOR)
xxx(MPI_DOUBLE, MPI_LXOR)
#endif
xxx(MPI_DOUBLE, MPI_BAND)
xxx(MPI_DOUBLE, MPI_BOR)
xxx(MPI_DOUBLE, MPI_BXOR)
#ifdef USE_ONLY_SPECIFIED_REDUCE_OPS
xxx(MPI_FLOAT, MPI_LAND)
xxx(MPI_FLOAT, MPI_LOR)
xxx(MPI_FLOAT, MPI_LXOR)
#endif
xxx(MPI_FLOAT, MPI_BAND)
xxx(MPI_FLOAT, MPI_BOR)
xxx(MPI_FLOAT, MPI_BXOR)


/* report overall results  */
MPITEST_report(pass,fail, verify, testname);
      
MPI_Finalize();
return fail;
} /* main() */
