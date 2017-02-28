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
                         Error test for MPI_Cancel()

This test verifies that the correct error is returned if MPI_Cancel is
called with an invalid argument.

MPI_Cancel error tests
-----------------------------------
1)  Call with inactive request............[MPI_ERR_REQUEST]
2)  Call with freed request...............[MPI_ERR_REQUEST]

In all cases, expect to receive appropriate error.

Rank 0 will first MPI_send_init() an integer to rank 1.  It will then do 
an MPI_Wait() making sure that the send has been completed and therefore the
output request handle becomes inactive.  It then call MPI_Cancel() with the
inactive request handle.

All other rank(s) will simply do nothing.

MPI Calls dependencies for this test:
  MPI_Send_init(), MPI_Recv(), MPI_Cancel(), MPI_Init(), MPI_Finalize()
  MPI_Error_string(), 
  [MPI_Allreduce(), MPI_Comm_rank(), MPI_Comm_size()]

Test history:
   1  06/18/96     simont       Original version
******************************************************************************/
#include <limits.h>

#include "mpitest_cfg.h"
#include "mpitest.h"

/* Data to be sent */
#define MPITEST_DATA_SEND     94

int main(int argc, char *argv[])
{
    int
        dest,	        /* Destination of Send message                       */
        pass, fail,	/* counts total number # of failures                 */
        ierr, ierr2,    /* return value from MPI calls                       */
        errorclass,	/* error class of ierr                               */
        source,
        size,
        count,
        tag;

    int
        buffer[10];	/* buffer space                                      */

    char
        testname[128],	/* the name of this test                             */
        info_buf[256];	/* for sprintf                                       */
    char error_string[MPI_MAX_ERROR_STRING];

    MPI_Status status;  /* Output status for MPI_Recv                        */

    MPI_Request request; /* Output request for non-blokcing request          */

    /*-----------------------------------------------------------------------*/

    /*   
    **  Initialize the MPI environment and test environment.
    */

    ierr = MPI_Init(&argc, &argv);
    if (ierr != MPI_SUCCESS) {
       sprintf(info_buf, "MPI_Init() returned %d", ierr);
       MPITEST_message(MPITEST_FATAL, info_buf);
    }

    sprintf(testname, "MPI_Cancel_err1: Inactive request");

    MPITEST_init(argc, argv);
    if (MPITEST_me == 0) {
       sprintf(info_buf, "Starting %s test", testname);
       MPITEST_message(MPITEST_INFO0, info_buf);
    }

    if (MPITEST_nump < 2)
       MPITEST_message(MPITEST_FATAL, "This test requires at least 2 ranks");

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

    /* Let all nodes send to rank 0  */

    dest = 1;
    count = 1;
    tag = 1;
    source = 0;

    if (MPITEST_me == source) {
       /* Initialize send data */
       buffer[0] = MPITEST_DATA_SEND;

       /* Send data */
       sprintf(info_buf, "Sending message from source %d to %d, tag: %d", source, dest, tag);
       MPITEST_message(MPITEST_INFO1, info_buf);

       ierr = MPI_Send_init(buffer, count, MPI_INT, dest, tag, MPI_COMM_WORLD, &request);
       if (ierr != MPI_SUCCESS) {
	  fail++;
	  sprintf(info_buf, "MPI_Isend() returned %d", ierr);
	  MPITEST_message(MPITEST_NONFATAL, info_buf);
	  MPI_Error_string(ierr, error_string, &size);
	  MPITEST_message(MPITEST_FATAL, error_string);
       }

       MPI_Start(&request);

       sprintf(info_buf, "Waiting for send request to be completed");
       MPITEST_message(MPITEST_INFO1, info_buf);

       ierr = MPI_Wait(&request, &status);
       if (ierr != MPI_SUCCESS) {
	  fail++;
	  sprintf(info_buf, "MPI_Wait() returned %d", ierr);
	  MPITEST_message(MPITEST_NONFATAL, info_buf);
	  MPI_Error_string(ierr, error_string, &size);
	  MPITEST_message(MPITEST_FATAL, error_string);
       }

       /* Cancelling an inactive request */
       sprintf(info_buf, "Cancelling an inactive request ...");
       MPITEST_message(MPITEST_INFO1, info_buf);

       ierr2 = MPI_Cancel(&request);
       if (ierr2 == MPI_SUCCESS) {
	  fail++;
	  sprintf(info_buf, "MPI_Cancel() returned MPI_SUCCESS with inactive request input");
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
	  if (errorclass != MPI_ERR_REQUEST) {
	     fail++;
	     sprintf(info_buf, "MPI_Cancel() /w inactive request returned error class %d, Expecting MPI_ERR_REQUEST (%d)",
		  errorclass, MPI_ERR_REQUEST);
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
       MPI_Request_free(&request);
    }
    else if (MPITEST_me == dest) {
       /* Receive the message source sent */
       sprintf(info_buf, "Receiving message from source: %d to %d, tag: %d", source, dest, tag);
       MPITEST_message(MPITEST_INFO1, info_buf);

       ierr = MPI_Recv(buffer, count, MPI_INT, source, tag, MPI_COMM_WORLD, &status);
       if (ierr != MPI_SUCCESS) {
	  fail++;
	  sprintf(info_buf, "MPI_Recv() returned %d", ierr);
	  MPITEST_message(MPITEST_NONFATAL, info_buf);
	  MPI_Error_string(ierr, error_string, &size);
	  MPITEST_message(MPITEST_FATAL, error_string);
       }
    }

    /* report overall results  */
    MPITEST_report(pass, fail, 0, testname);

    ierr = MPI_Finalize();

    return fail;
}/* main() */
