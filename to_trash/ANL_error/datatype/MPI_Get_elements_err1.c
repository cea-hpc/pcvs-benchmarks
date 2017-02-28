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
                     Error test for MPI_Get_elements()

This test verifies that the correct error is returned if MPI_Get_elements()
is called with an invalid argument.

MPI_Get_elements error tests
-----------------------------------
1)  Call with MPI_DATATYPE_NULL...........[MPI_ERR_TYPE]
2)  Call with uncommitted datatype........[MPI_ERR_TYPE]

In all cases, expect to receive appropriate error.

Rank 0 will first call call MPI_Send() to send a message to
rank 1.  Rank 1 will then call MPI_Recv() to receive to meesage
sent.  It will then call MPI_Get_elements() with MPI_DATATYPE_NULL.

The resulting erro code will then be checked and the corresponding
error class will be verified to make sure it is MPI_ERR_TYPE.

All other rank(s) will simply do nothing.

MPI Calls dependencies for this test:
  MPI_Get_elements(), MPI_Init(), MPI_Finalize()
  MPI_Error_string(), MPI_Send(), MPI_Recv()
  [MPI_Allreduce(), MPI_Comm_rank(), MPI_Comm_size()]

Test history:
   1  06/27/96     simont       Original version
******************************************************************************/
#include <limits.h>

#include "mpitest_cfg.h"
#include "mpitest.h"

#define MPITEST_BUFSIZE  10
#define MPITEST_TAG      10

int main(int argc, char *argv[])
{
    int
        dest,	        /* Destination of Send message                       */
        pass, fail,	/* counts total number # of failures                 */
        ierr, ierr2,    /* return value from MPI calls                       */
        errorclass,	/* error class of ierr                               */
        source,
        size,
        count = MPITEST_BUFSIZE,
        elmt_count;


    int buffer[MPITEST_BUFSIZE], i;

    char
        testname[128],	/* the name of this test                             */
        info_buf[256];	/* for sprintf                                       */
    char error_string[MPI_MAX_ERROR_STRING];

    MPI_Datatype type;

    MPI_Status status;

    /*-----------------------------------------------------------------------*/

    /*   
    **  Initialize the MPI environment and test environment.
    */

    ierr = MPI_Init(&argc, &argv);
    if (ierr != MPI_SUCCESS) {
       sprintf(info_buf, "MPI_Init() returned %d", ierr);
       MPITEST_message(MPITEST_FATAL, info_buf);
    }

    sprintf(testname, "MPI_Get_elements_err1: MPI_DATATYPE_NULL");

    MPITEST_init(argc, argv);
    if (MPITEST_me == 0) {
       sprintf(info_buf, "Starting %s test", testname);
       MPITEST_message(MPITEST_INFO0, info_buf);
    }

    pass = 0;
    fail = 0;

    if (MPITEST_nump < 2) {
       MPITEST_message(MPITEST_FATAL, "This test requires at least 2 ranks");
    }

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
    dest = 1;
    type = MPI_INT;

    if (MPITEST_me == source) {
      /* Init send buffer */
      for (i = 0; i < MPITEST_BUFSIZE; i ++) {
	buffer[i] = i;
      }

      /* Sending data from source to dest */
      sprintf(info_buf, "Sending message from source %d to %d", source, dest);
      MPITEST_message(MPITEST_INFO1, info_buf);

      ierr = MPI_Send(buffer, count, type, dest, MPITEST_TAG, MPI_COMM_WORLD);
      if (ierr != MPI_SUCCESS) {
	fail++;
	sprintf(info_buf, "MPI_Send() returned %d", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_FATAL, error_string);
      }
    }
    else if (MPITEST_me == dest) {
      /* Receiving data from source */
      sprintf(info_buf, "Receiving message from source %d to %d", source, dest);
      MPITEST_message(MPITEST_INFO1, info_buf);

      ierr = MPI_Recv(buffer, count, type, source, MPITEST_TAG, MPI_COMM_WORLD, &status);
      if (ierr != MPI_SUCCESS) {
	fail++;
	sprintf(info_buf, "MPI_Recv() returned %d", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_FATAL, error_string);
      }

      /* Not checking receive buffer! */

      /* MPI_Get_elements() with MPI_DATATYPE_NULL */
      type = MPI_DATATYPE_NULL;
      ierr2 = MPI_Get_elements(&status, type, &elmt_count);
      if (ierr2 == MPI_SUCCESS) {
	fail++;
	sprintf(info_buf, "MPI_Get_elements() with MPI_DATATYPE_NULL returned MPI_SUCCESS");
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
	else if (errorclass != MPI_ERR_TYPE) {
	  fail++;
	  sprintf(info_buf, "MPI_Get_elements() with MPI_DATATYPE_NULL returned error class %d, expected MPI_ERR_COUNT", errorclass);
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
