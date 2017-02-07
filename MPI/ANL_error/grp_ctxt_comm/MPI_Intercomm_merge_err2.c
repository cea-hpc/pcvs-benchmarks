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
                     Error test for MPI_Intercomm_merge()

This test verifies that the correct error is returned if MPI_Intercomm_merge()
is called with invalid arguments.

MPI_Intercomm_merge error tests
-----------------------------------
1)  Call with MPI_COMM_NULL.....................................[MPI_ERR_COMM]
2)  Call with an intercommunicator..............................[MPI_ERR_ARG/MPI_ERR_COMM/MPI_ERR_OTHER]
3)  Call with different values of high withing a single group...[MPI_ERR_ARG/MPI_ERR_OTHER]

In all cases, expect to receive appropriate error.

MPI Calls dependencies for this test:
  MPI_Intercomm_merge(), MPI_Init(), MPI_Finalize()
  MPI_Error_string(), MPI_Comm_split(), MPI_Comm_dup(),
  MPI_Intercomm_create(),
  [MPI_Allreduce(), MPI_Comm_size(), MPI_Comm_rank()]

Test history:
   1  08/05/96     brdavis      Original version
******************************************************************************/
#include <limits.h>

#include "mpitest_cfg.h"
#include "mpitest.h"

#define TEST_NAME "MPI_Intercomm_merge_err2"
#define TEST_DESCRIPTION "intercommunicator as an intracommunicator (MPI_COMM_WORLD)"
#define EXPECTED_ERRORS "MPI_ERR_ARG or MPI_ERR_COMM or MPI_ERR_OTHER"
#define PROCS_NEEDED 2

int main(int argc, char *argv[])
{
    int
        pass, fail,        /* counts total number # of failures              */
        ierr, ierr2,    /* return value from MPI calls                       */
        errorclass,        /* error class of ierr                            */
        size;

    char
        testname[128],        /* the name of this test                             */
        info_buf[256];        /* for sprintf                                       */
    char error_string[MPI_MAX_ERROR_STRING];

    MPI_Comm
        intercomm,      /* Inter-communicator                                */
        newintracomm,   /* the new inter-communicator                        */
        local_comm,
        peer_comm;

    int
        high,           /* Used to help determine ordering withing the new   */
                        /* communicator.                                     */
        color,          /* color used by MPI_Comm_split to make local_comms  */
        key,            /* used to control ordering in new local_comms       */
        local_leader,
        remote_leader,
        tag;

    /*-----------------------------------------------------------------------*/

    /*   
    **  Initialize the MPI environment and test environment.
    */

    ierr = MPI_Init(&argc, &argv);
    if (ierr != MPI_SUCCESS) {
       sprintf(info_buf, "MPI_Init() returned %d", ierr);
       MPITEST_message(MPITEST_FATAL, info_buf);
    }

    sprintf(testname, "%s: %s", TEST_NAME, TEST_DESCRIPTION);

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

    /* Set values of high */
    high = 0;

    /* change some of the values to generate the desired errors */
    intercomm = MPI_COMM_WORLD;

    /* Calling MPI_Intercomm_merge with bad arguments */
    sprintf(info_buf, "Calling MPI_Intercomm_merge with %s", TEST_DESCRIPTION);
    MPITEST_message(MPITEST_INFO1, info_buf);

    ierr2 = MPI_Intercomm_merge(intercomm, high, &newintracomm);
    if (ierr2 == MPI_SUCCESS) {
      fail++;
      sprintf(info_buf, "MPI_Intercomm_merge() returned MPI_SUCCESS");
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
      else if ((errorclass != MPI_ERR_ARG) && (errorclass != MPI_ERR_COMM) && (errorclass != MPI_ERR_OTHER)){
        fail++;
        sprintf(info_buf, "MPI_Intercomm_merge() with %s returned error class %d, expected %s", TEST_DESCRIPTION, errorclass, EXPECTED_ERRORS);
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
