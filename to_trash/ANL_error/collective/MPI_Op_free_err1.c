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
*                          Error test for MPI_Op_free
*
* This test verifies that the correct error is returned if MPI_Op_free is
* called with an invalid argument.
*
* 1)  Use MPI_OP_NULL.
* 2)  Use a pre-defined operator.
*
* In all cases, expect to receive MPI_ERR_OP
*
* Test history:
*    1  01/12/96     gt   Created
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

  int
    count;

  char
    testname[128],     /*  the name of this test           */
    info_buf[256];     /*  for fprintf                     */
    char error_string[MPI_MAX_ERROR_STRING];

 MPI_Op
    op;

/*
**  Initialize the MPI environment and test environment.
*/
ierr = MPI_Init(&argc, &argv);
if( ierr!=MPI_SUCCESS)
  {
    sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr); 
    MPITEST_message(MPITEST_FATAL, info_buf);
  }

sprintf( testname, "MPI_Op_free_err1");
MPITEST_init( argc, argv);

pass = 0;
fail = 0;
verify = 0;


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
**  Invalid operator.
*/
sprintf(info_buf, "about to call MPI_Op_free w/ MPI_OP_NULL");
MPITEST_message( MPITEST_INFO1, info_buf);

op=MPI_OP_NULL;
ierr = MPI_Op_free(&op);


if (ierr == MPI_SUCCESS)
  {
    fail++;
    sprintf(info_buf, "MPI_Op_free w/ MPI_OP_NULL did not FAIL");
    MPITEST_message(MPITEST_NONFATAL, info_buf);
  }
else
  {
    MPI_Error_class(ierr, &errorclass);
    if (errorclass != MPI_ERR_OP)
      {
	fail++;
	sprintf(info_buf, "MPI_OP_free w/ MPI_OP_NULL returned %d, expected MPI_ERR_OP",
	    errorclass);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message( MPITEST_NONFATAL, info_buf);
      }
    else
      {
	pass++;
	sprintf(info_buf, "ierr = %d, errorclass = %d", ierr,
	     errorclass);
	MPITEST_message( MPITEST_INFO2, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message( MPITEST_INFO1, info_buf);
      }
  }

/* report overall results  */
MPITEST_report(pass,fail, verify, testname);

MPI_Finalize();
return fail;
} /* main() */