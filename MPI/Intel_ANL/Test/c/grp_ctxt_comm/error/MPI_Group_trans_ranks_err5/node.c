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
                     Error test for MPI_Group_translate_ranks()

This test verifies that the correct error is returned if MPI_Group_translate_ranks()
is called with invalid arguments.

MPI_Group_translate_ranks error tests
-----------------------------------
1)  Call with group1=MPI_GROUP_NULL..................[MPI_ERR_GROUP]
2)  Call with group2=MPI_GROUP_NULL..................[MPI_ERR_GROUP]
3)  Call with negative n.............................[MPI_ERR_OTHER/MPI_ERR_ARG]
4)  Call with a negative argument in ranks1..........[MPI_ERR_RANK]
5)  Call with an argument in ranks1 which is too big.[MPI_ERR_RANK]

In all cases, expect to receive appropriate error.

Rank 0 will call MPI_Group_translate_ranks with an argument in ranks1
which is too big.  The resulting error code will then be checked and
the corresponding error class will be verified to make sure it is
MPI_ERR_RANK.

All other rank(s) will simply do nothing.

MPI Calls dependencies for this test:
  MPI_Group_translate_ranks(), MPI_Init(), MPI_Finalize()
  MPI_Error_string(), MPI_Comm_group(),
  [MPI_Allreduce(), MPI_Comm_rank(), MPI_Comm_size()]

Test history:
   1  08/05/96     brdavis      Original version
******************************************************************************/
#include <limits.h>

#include "mpitest_cfg.h"
#include "mpitest.h"

#define RANKS_SIZE 10

int main(int argc, char *argv[])
{
    int
        pass, fail,	/* counts total number # of failures                 */
        ierr, ierr2,    /* return value from MPI calls                       */
        errorclass,	/* error class of ierr                               */
        size,
        n,
        ranks1[RANKS_SIZE],
        ranks2[RANKS_SIZE];

    char
        testname[128],	/* the name of this test                             */
        info_buf[256];	/* for sprintf                                       */
    char error_string[MPI_MAX_ERROR_STRING];

    MPI_Group group1, group2;

    /*-----------------------------------------------------------------------*/

    /*   
    **  Initialize the MPI environment and test environment.
    */

    ierr = MPI_Init(&argc, &argv);
    if (ierr != MPI_SUCCESS) {
       sprintf(info_buf, "MPI_Init() returned %d", ierr);
       MPITEST_message(MPITEST_FATAL, info_buf);
    }

    sprintf(testname, "MPI_Group_translate_ranks_err5: an argument in ranks1 which is too big");

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

    /* Initalize all the variables */
    /* group1 */
    ierr = MPI_Comm_group(MPI_COMM_WORLD, &group1);
    if (ierr != MPI_SUCCESS) {
       fail++;
       sprintf(info_buf, "MPI_Comm_group returned %d", ierr);
       MPITEST_message(MPITEST_NONFATAL, info_buf);
       MPI_Error_string(ierr, error_string, &size);
       MPITEST_message(MPITEST_FATAL, error_string);
    }
    /* group2 */
    ierr = MPI_Comm_group(MPI_COMM_WORLD, &group2);
    if (ierr != MPI_SUCCESS) {
       fail++;
       sprintf(info_buf, "MPI_Comm_group returned %d", ierr);
       MPITEST_message(MPITEST_NONFATAL, info_buf);
       MPI_Error_string(ierr, error_string, &size);
       MPITEST_message(MPITEST_FATAL, error_string);
    }
    /* n */
    n = 1;
    /* ranks1 */
    ranks1[0] = MPITEST_nump;

    if (MPITEST_me == 0) {
      /* Calling MPI_Group_translate_ranks with bad arguments */
      sprintf(info_buf, "Calling MPI_Group_translate_ranks with an argument in ranks1 which is too big");
      MPITEST_message(MPITEST_INFO1, info_buf);

      ierr2 = MPI_Group_translate_ranks(group1, n, ranks1, group2, ranks2);
      if (ierr2 == MPI_SUCCESS) {
	fail++;
	sprintf(info_buf, "MPI_Group_translate_ranks() returned MPI_SUCCESS");
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
	else if (errorclass != MPI_ERR_RANK) {
	  fail++;
          sprintf(info_buf, "MPI_Group_translate_ranks() with an argument in ranks1 which is too big returned error class %d, expected MPI_ERR_RANK", errorclass);
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

    ierr = MPI_Group_free(&group1);
    if (ierr != MPI_SUCCESS) {
       fail++;
       sprintf(info_buf, "MPI_Group_free returned %d", ierr);
       MPITEST_message(MPITEST_NONFATAL, info_buf);
       MPI_Error_string(ierr, error_string, &size);
       MPITEST_message(MPITEST_FATAL, error_string);
    }
    ierr = MPI_Group_free(&group2);
    if (ierr != MPI_SUCCESS) {
       fail++;
       sprintf(info_buf, "MPI_Group_free returned %d", ierr);
       MPITEST_message(MPITEST_NONFATAL, info_buf);
       MPI_Error_string(ierr, error_string, &size);
       MPITEST_message(MPITEST_FATAL, error_string);
    }

    /* report overall results  */
    MPITEST_report(pass, fail, 0, testname);

    ierr = MPI_Finalize();

    return fail;
}/* main() */
