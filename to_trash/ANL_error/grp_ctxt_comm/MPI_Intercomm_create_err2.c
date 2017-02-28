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
                     Error test for MPI_Intercomm_create()

This test verifies that the correct error is returned if MPI_Intercomm_create()
is called with invalid arguments.

MPI_Intercomm_create error tests
-----------------------------------
1)  Call with local_comm=MPI_COMM_NULL............................[MPI_ERR_COMM]
2)  Call with peer_comm=MPI_COMM_NULL.............................[MPI_ERR_COMM]
3)  removed
4)  removed
5)  Call with peer_comm not containing local and remote leaders...[MPI_ERR_ARG/MPI_ERR_COMM/MPI_ERR_OTHER]
6)  removed
7)  Call with local_leader=MPI_ANY_SOURCE.........................[MPI_ERR_ARG/MPI_ERR_OTHER]
8)  Call with remote_leader=MPI_ANY_SOURCE........................[MPI_ERR_ARG/MPI_ERR_OTHER]
9)  Call with local_leader not a valid rank in local_comm.........[MPI_ERR_RANK]
10) Call with remote_leader not a valid rank in peer_comm.........[MPI_ERR_RANK]

We only run on two nodes for this test because there is no requirement
that peer_comm be valid for nodes other then the leaders so the other
nodes down't need to check it.  As a result some implemenations won't
check for peer_comm=MPI_COMM_NULL in other nodes and they will hang
waiting for communication from the leaders (who have exited with
errors.)

In all cases, expect to receive appropriate error.

MPI Calls dependencies for this test:
  MPI_Intercomm_create(), MPI_Init(), MPI_Finalize()
  MPI_Error_string(), MPI_Comm_split(), MPI_Comm_dup(),
  [MPI_Allreduce(), MPI_Comm_size(), MPI_Comm_rank()]

Test history:
   1  08/05/96     brdavis      Original version
******************************************************************************/
#include <limits.h>

#include "mpitest_cfg.h"
#include "mpitest.h"

#define TEST_NAME "MPI_Intercomm_create_err2"
#define TEST_DESCRIPTION "peer_comm=MPI_COMM_NULL"
#define EXPECTED_ERRORS "MPI_ERR_COMM"
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
        local_comm,     /* local intra-communicator                          */
        peer_comm,      /* "peer" communicator                               */
        newintercomm;   /* the new inter-communicator                        */

    int
        local_leader,   /* rank of the local group leader in local_comm;     */
                        /* significant only at the local_leader              */
        remote_leader,  /* rank of the remote leader in peer_comm;           */
                        /* significant only at the local_leader              */
        tag,            /* "safe" tag                                        */
        color,          /* color used by MPI_Comm_split to make local_comms  */
        key;            /* used to control ordering in new local_comms       */

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

    if (MPITEST_nump < PROCS_NEEDED) {
       sprintf(info_buf, "Not enough ranks to run %s, need at least %d", TEST_NAME, PROCS_NEEDED);
      MPITEST_message(MPITEST_FATAL, info_buf);
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

    /* Initalize all the values for the bad calls */
    /* set local_comm -- MPI_COMM_SELF works */
    local_comm = MPI_COMM_SELF;
    /* set local_leader -- all are 0 due to the use of keys above */
    local_leader = 0;
    /* set peer_comm to the bad value */
    peer_comm = MPI_COMM_NULL;
    /* set remote_leader -- first and last ranks in MPI_COMM_WORLD */
    if (MPITEST_me == 0)
       remote_leader = 1;
    else
       remote_leader = 0;
    /* choose a tag -- anything will do since we only need one */
    tag = 42;
    /* setup errorhandlers on local_comm so the tests work */
    ierr = MPI_Errhandler_set(local_comm, MPI_ERRORS_RETURN);
    if (ierr != MPI_SUCCESS) {
       fail++;
       sprintf(info_buf, "MPI_Errorhandler_set returned %d", ierr);
       MPITEST_message(MPITEST_NONFATAL, info_buf);
       MPI_Error_string(ierr, error_string, &size);
       MPITEST_message(MPITEST_FATAL, error_string);
    }

    if(MPITEST_me < 2)
    {
      /* Calling MPI_Intercomm_create with bad arguments */
      sprintf(info_buf, "Calling MPI_Intercomm_create with %s", TEST_DESCRIPTION);
      MPITEST_message(MPITEST_INFO1, info_buf);

      ierr2 = MPI_Intercomm_create(local_comm, local_leader, peer_comm, remote_leader, tag, &newintercomm);
      if (ierr2 == MPI_SUCCESS) {
        fail++;
        sprintf(info_buf, "MPI_Intercomm_create() returned MPI_SUCCESS");
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
        else if (errorclass != MPI_ERR_COMM) {
          fail++;
          sprintf(info_buf, "MPI_Intercomm_create() with %s returned error class %d, expected %s", TEST_DESCRIPTION, errorclass, EXPECTED_ERRORS);
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
