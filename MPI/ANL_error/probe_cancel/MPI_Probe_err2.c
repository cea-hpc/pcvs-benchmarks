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
                         Error test for MPI_Probe()

This test verifies that the correct error is returned if MPI_Probe is
called with an invalid argument.


Blocking probe error tests
-----------------------------------
1)  Negative source rank ........... [MPI_ERR_RANK]
2)  Negative tag ................... [MPI_ERR_TAG]
3)  tag > MPI_TAG_UB ............... [MPI_ERR_TAG]
4)  Call with MPI_COMM_NULL ........ [MPI_ERR_COMM]
5)  source rank = comm_size  ....... [MPI_ERR_RANK]

MPI Calls dependencies for this test:
  MPI_Probe(), MPI_Init, MPI_Finalize(), MPI_Error_string()
  MPI_Error_class(), MPI_Isend()
  [MPI_Allreduce(), MPI_Comm_rank(), MPI_Comm_size()]

In all cases, expect to receive appropriate error.

Test history:
   1  05/29/96     simont       Original version

******************************************************************************/
#include "mpitest_cfg.h"
#include "mpitest.h"

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
        tag,
        bad_tag;

    char
        testname[128],	/* the name of this test                             */
        info_buf[256];	/* for sprintf                                       */
    char error_string[MPI_MAX_ERROR_STRING];

    MPI_Status status;  /* Output status for MPI_Probe                       */

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

    bad_tag = -2; /* bad tag */
    if (bad_tag == MPI_ANY_TAG) bad_tag--;

    sprintf(testname, "MPI_Probe_err2: tag negative (%d)", bad_tag);
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
       /* MPI_Probe call with the negative tag */
       sprintf(info_buf, "Calling MPI_Probe() with negative tag (%d)", bad_tag);
       MPITEST_message(MPITEST_INFO1, info_buf);

       ierr2 = MPI_Probe(source, bad_tag, MPI_COMM_WORLD, &status);

       if (ierr2 == MPI_SUCCESS) {
	  fail++;
	  sprintf(info_buf, "MPI_Probe() with negative tag (%d) returned MPI_SUCCESS", bad_tag);
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
	  else if (errorclass != MPI_ERR_TAG) {
	     fail++;
	     sprintf(info_buf, "MPI_Probe() /w negative tag (%d) returned error class %d, expected MPI_ERR_TAG",
		     bad_tag, errorclass);
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

    /* report overall results  */
    MPITEST_report(pass, fail, 0, testname);

    ierr = MPI_Finalize();

    return fail;
}/* main() */
