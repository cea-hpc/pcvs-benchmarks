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
/******************************************************************************
                     Error test for MPI_Pack()

This test verifies that the correct error is returned if MPI_Pack()
is called with an invalid argument.

MPI_Pack error tests
-----------------------------------
1)  Call with negative input count........[MPI_ERR_COUNT]
2)  Call with negative outsize............[MPI_ERR_OTHER/MPI_ERR_ARG]
3)  Call with MPI_COMM_NULL...............[MPI_ERR_COMM]
4)  Call with MPI_DATATYPE_NULL...........[MPI_ERR_TYPE]
5)  Call with uncommitted datatype........[MPI_ERR_TYPE]
6)  Call with outsize < incount with
    basic type............................[MPI_ERR_COUNT/MPI_ERR_OTHER/MPI_ERR_ARG]
 
In all cases, expect to receive appropriate error.

Rank 0 will first call MPI_Pack with outsize < incount using MPI_INT.
The resulting erro code will then be checked and the corresponding
error class will be verified to make sure it is MPI_ERR_OTHER or
MPI_ERR_ARG.

All other rank(s) will simply do nothing.

MPI Calls dependencies for this test:
  MPI_Pack(), MPI_Init(), MPI_Finalize()
  MPI_Error_string(), 
  [MPI_Allreduce(), MPI_Comm_rank(), MPI_Comm_size()]

Test history:
   1  06/27/96     simont       Original version
******************************************************************************/
#include <limits.h>

#include "mpitest_cfg.h"
#include "mpitest.h"

#define MPITEST_BUFSIZE  10

int main(int argc, char *argv[])
{
    int
        dest,	        /* Destination of Send message                       */
        pass, fail,	/* counts total number # of failures                 */
        ierr, ierr2,    /* return value from MPI calls                       */
        errorclass,	/* error class of ierr                               */
        source,
        size,
        incount,
        position,
        outsize;

    int inbuf[MPITEST_BUFSIZE], outbuf[MPITEST_BUFSIZE];

    char
        testname[128],	/* the name of this test                             */
        info_buf[256];	/* for sprintf                                       */
    char error_string[MPI_MAX_ERROR_STRING];

    MPI_Datatype type;

    /*-----------------------------------------------------------------------*/

    /*   
    **  Initialize the MPI environment and test environment.
    */

    ierr = MPI_Init(&argc, &argv);
    if (ierr != MPI_SUCCESS) {
       sprintf(info_buf, "MPI_Init() returned %d", ierr);
       MPITEST_message(MPITEST_FATAL, info_buf);
    }

    sprintf(testname, "MPI_Pack_err6: outsize < incount for basic type");

    MPITEST_init(argc, argv);
    if (MPITEST_me == 0) {
       sprintf(info_buf, "Starting %s test", testname);
       MPITEST_message(MPITEST_INFO0, info_buf);
    }

    pass = 0;
    fail = 0;

    /* Set an errorhandler so we get control back. */
    ierr = MPI_Errhandler_set(MPI_COMM_WORLD, MPI_ERRORS_RETURN);
    if (ierr != MPI_SUCCESS) {
       fail++;
       sprintf(info_buf, "MPI_Errorhandler_set returned %d", ierr);
       MPITEST_message(MPITEST_NONFATAL, info_buf);
       MPI_Error_string(ierr, error_string, &size);
       MPITEST_message(MPITEST_FATAL, error_string);
    }

    source = 0;

    if (MPITEST_me == source) {
      incount = MPITEST_BUFSIZE;
      outsize = MPITEST_BUFSIZE - 1;
      type = MPI_INT;
      position = 0;

      /* MPI_Pack() with outsize < incount for basic type */
      ierr2 = MPI_Pack(inbuf, incount, type, outbuf, outsize, &position, MPI_COMM_WORLD);

      if (ierr2 == MPI_SUCCESS) {
	fail++;
	sprintf(info_buf, "MPI_Pack() with outsize < incount for basic type returned MPI_SUCCESS");
	MPITEST_message(MPITEST_NONFATAL, info_buf);
      }
      else {
	ierr = MPI_Error_class(ierr2, &errorclass);
	if (ierr != MPI_SUCCESS) {
	  fail++;
	  sprintf(info_buf, "MPI_Error_class() returned %d", ierr);
	  MPITEST_message(MPITEST_NONFATAL, info_buf);
	  MPI_Error_string(ierr, error_string, &size);
	  MPITEST_message(MPITEST_FATAL, error_string);
	}
	else if ((errorclass != MPI_ERR_COUNT) && (errorclass != MPI_ERR_OTHER) && (errorclass != MPI_ERR_ARG)) {
	  fail++;
	  sprintf(info_buf, "MPI_Pack() with outsize < incount for basic type returned error class %d (%s), expected MPI_ERR_COUNT or MPI_ERR_OTHER or MPI_ERR_ARG", errorclass, MPITEST_GetErrName( errorclass ) );
	  MPITEST_message(MPITEST_NONFATAL, info_buf);
	  MPI_Error_string(ierr2, error_string, &size);
	  MPITEST_message(MPITEST_NONFATAL, error_string);
	}
	else {
	  pass++;
	  sprintf(info_buf, "ierr = %d, errorclass = %d", ierr2,
		  errorclass);
	  MPITEST_message(MPITEST_INFO2, info_buf);
	  MPI_Error_string(ierr2, error_string, &size);
	  MPITEST_message(MPITEST_INFO1, error_string);
	}
      }
    }

    /* report overall results  */
    MPITEST_report(pass, fail, 0, testname);

    ierr = MPI_Finalize();

    return fail;
}/* main() */
