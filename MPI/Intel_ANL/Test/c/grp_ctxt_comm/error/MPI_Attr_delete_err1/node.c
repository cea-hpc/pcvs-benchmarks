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
                     Error test for MPI_Attr_delete()

This test verifies that the correct error is returned if MPI_Attr_delete()
is called with invalid arguments.

MPI_Attr_delete error tests
-----------------------------------
1)  Call with MPI_COMM_NULL..............[MPI_ERR_COMM]
1)  Call with MPI_KEYVAL_INVALID.........[MPI_ERR_ARG/MPI_ERR_OTHER]

In all cases, expect to receive appropriate error.

Rank 0 will call MPI_Attr_delete with MPI_COMM_NULL.
The resulting error code will then be checked and the corresponding
error class will be verified to make sure it is MPI_ERR_COMM.

All other rank(s) will simply do nothing.

MPI Calls dependencies for this test:
  MPI_Attr_delete(), MPI_Init(), MPI_Finalize()
  MPI_Error_string()
  [MPI_Allreduce(), MPI_Comm_size(), MPI_Comm_rank()]

Test history:
   1  08/05/96     brdavis      Original version
******************************************************************************/
#include <limits.h>

#include "mpitest_cfg.h"
#include "mpitest.h"

int main(int argc, char *argv[])
{
    int
        pass, fail,	/* counts total number # of failures                 */
        ierr, ierr2,    /* return value from MPI calls                       */
        errorclass,	/* error class of ierr                               */
        size,
        keyval;

    char
        testname[128],	/* the name of this test                             */
        info_buf[256];	/* for sprintf                                       */
    char error_string[MPI_MAX_ERROR_STRING];

    /*-----------------------------------------------------------------------*/

    /*   
    **  Initialize the MPI environment and test environment.
    */

    ierr = MPI_Init(&argc, &argv);
    if (ierr != MPI_SUCCESS) {
       sprintf(info_buf, "MPI_Init() returned %d", ierr);
       MPITEST_message(MPITEST_FATAL, info_buf);
    }

    sprintf(testname, "MPI_Attr_delete_err1: MPI_COMM_NULL");

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

    /* create a valid keyval to insure exposure of the communicator error */
    ierr = MPI_Keyval_create(MPI_NULL_COPY_FN, MPI_NULL_DELETE_FN, &keyval, 0);
    if (ierr != MPI_SUCCESS) {
       fail++;
       sprintf(info_buf, "MPI_Keyval_create returned %d", ierr);
       MPITEST_message(MPITEST_NONFATAL, info_buf);
       MPI_Error_string(ierr, error_string, &size);
       MPITEST_message(MPITEST_FATAL, error_string);
    }

    if (MPITEST_me == 0) {
      /* Calling MPI_Attr_delete with MPI_COMM_NULL */
      sprintf(info_buf, "Calling MPI_Attr_delete with MPI_COMM_NULL");
      MPITEST_message(MPITEST_INFO1, info_buf);

      ierr2 = MPI_Attr_delete(MPI_COMM_NULL, keyval);
      if (ierr2 == MPI_SUCCESS) {
	fail++;
	sprintf(info_buf, "MPI_Attr_delete() returned MPI_SUCCESS");
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
          sprintf(info_buf, "MPI_Attr_delete() with MPI_COMM_NULL returned error class %d, expected MPI_ERR_COMM", errorclass);
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

    /* free the keyval */
    ierr = MPI_Keyval_free(&keyval);
    if (ierr != MPI_SUCCESS) {
       fail++;
       sprintf(info_buf, "MPI_Keyval_free returned %d", ierr);
       MPITEST_message(MPITEST_NONFATAL, info_buf);
       MPI_Error_string(ierr, error_string, &size);
       MPITEST_message(MPITEST_FATAL, error_string);
    }

    /* report overall results  */
    MPITEST_report(pass, fail, 0, testname);

    ierr = MPI_Finalize();

    return fail;
}/* main() */
