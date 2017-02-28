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
                     Test for MPI_Comm_compare()

This test verifies that the correct result is returned if MPI_Comm_compare()
is called with MPI_COMM_NULL.

10-31-02: The correct result is to indicate an error.  See MPI-1, 
2.4.1 on Opaque Objects and the absence of anything in 5.4.1 (Communicator
Accessors) that allows a null handle.

MPI Calls dependencies for this test:
  MPI_Comm_compare(), MPI_Init(), MPI_Finalize()
  MPI_Error_string(),
  [MPI_Allreduce(), MPI_Comm_compare(), MPI_Comm_compare()]

Test history:
   1  08/05/96     brdavis      Original version
******************************************************************************/
#include "mpitest_cfg.h"
#include "mpitest.h"

int main(int argc, char *argv[])
{
    int
        pass, fail,	/* counts total number # of failures                 */
        ierr,           /* return value from MPI calls                       */
        errorclass,	/* error class of ierr                               */
        size,
        result;

    char
        testname[128],	/* the name of this test                             */
        info_buf[256];	/* for sprintf                                       */
    char error_string[MPI_MAX_ERROR_STRING];

    MPI_Comm comm1, comm2;

    /*-----------------------------------------------------------------------*/

    /*   
    **  Initialize the MPI environment and test environment.
    */

    ierr = MPI_Init(&argc, &argv);
    if (ierr != MPI_SUCCESS) {
       sprintf(info_buf, "MPI_Init() returned %d", ierr);
       MPITEST_message(MPITEST_FATAL, info_buf);
    }

    sprintf(testname, "MPI_Comm_compare_null");

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
    /* Set an errorhandler so we get control back. */
    ierr = MPI_Errhandler_set(MPI_COMM_SELF, MPI_ERRORS_RETURN);
    if (ierr != MPI_SUCCESS) {
       fail++;
       sprintf(info_buf, "MPI_Errorhandler_set returned %d", ierr);
       MPITEST_message(MPITEST_NONFATAL, info_buf);
       MPI_Error_string(ierr, error_string, &size);
       MPITEST_message(MPITEST_FATAL, error_string);
    }

    /* comm1 */
    comm1 = MPI_COMM_NULL;
    /* comm2 */
    comm2 = MPI_COMM_WORLD;

    ierr = MPI_Comm_compare(comm1, comm2, &result);
    if (ierr == MPI_SUCCESS) {
	fail++;
	sprintf( info_buf, "MPI_Comm_compare(WORLD, NULL) returned success");
	MPITEST_message(MPITEST_NONFATAL, info_buf);
    }
#ifdef ORIGINAL_TEST
    if (ierr != MPI_SUCCESS) {
	fail++;
	sprintf(info_buf, "MPI_Comm_compare(WORLD, NULL) returned %d", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
        MPI_Error_string(ierr, error_string, &size);
        MPITEST_message(MPITEST_NONFATAL, error_string);
    } else if (result != MPI_UNEQUAL) {
        fail++;
        sprintf(info_buf, "MPI_Comm_compare(WORLD, NULL) result = %d, expected MPI_UNEQUAL", result);
        MPITEST_message(MPITEST_NONFATAL, info_buf);
    } 
#endif
    else pass++;

    /* comm1 */
    comm1 = MPI_COMM_WORLD;
    /* comm2 */
    comm2 = MPI_COMM_NULL;

    ierr = MPI_Comm_compare(comm1, comm2, &result);
    if (ierr == MPI_SUCCESS) {
	fail++;
	sprintf(info_buf, "MPI_Comm_compare(NULL, WORLD) returned success");
	MPITEST_message(MPITEST_NONFATAL, info_buf);
    }
#ifdef ORIGINAL_TEST
    if (ierr != MPI_SUCCESS) {
	fail++;
	sprintf(info_buf, "MPI_Comm_compare(NULL, WORLD) returned %d", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
        MPI_Error_string(ierr, error_string, &size);
        MPITEST_message(MPITEST_NONFATAL, error_string);
    } else if (result != MPI_UNEQUAL) {
        fail++;
        sprintf(info_buf, "MPI_Comm_compare(NULL, WORLD) result = %d, expected MPI_UNEQUAL", result);
        MPITEST_message(MPITEST_NONFATAL, info_buf);
    } 
#endif
    else pass++;

    /* comm1 */
    comm1 = MPI_COMM_SELF;
    /* comm2 */
    comm2 = MPI_COMM_NULL;

    ierr = MPI_Comm_compare(comm1, comm2, &result);
    if (ierr == MPI_SUCCESS) {
	fail++;
	sprintf(info_buf, "MPI_Comm_compare(SELF, NULL) returned success");
	MPITEST_message(MPITEST_NONFATAL, info_buf);
    }
#ifdef ORIGINAL_TEST
    if (ierr != MPI_SUCCESS) {
	fail++;
	sprintf(info_buf, "MPI_Comm_compare(SELF, NULL) returned %d", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
        MPI_Error_string(ierr, error_string, &size);
        MPITEST_message(MPITEST_NONFATAL, error_string);
    } else if (result != MPI_UNEQUAL) {
        fail++;
        sprintf(info_buf, "MPI_Comm_compare(SELF, NULL) result = %d, expected MPI_UNEQUAL", result);
        MPITEST_message(MPITEST_NONFATAL, info_buf);
    } 
#endif
    else pass++;

    /* comm1 */
    comm1 = MPI_COMM_NULL;
    /* comm2 */
    comm2 = MPI_COMM_SELF;

    ierr = MPI_Comm_compare(comm1, comm2, &result);
    if (ierr == MPI_SUCCESS) {
	fail++;
	sprintf(info_buf, "MPI_Comm_compare(NULL, SELF) returned success");
	MPITEST_message(MPITEST_NONFATAL, info_buf);
    }
#ifdef ORIGINAL_TEST
    if (ierr != MPI_SUCCESS) {
	fail++;
	sprintf(info_buf, "MPI_Comm_compare(NULL, SELF) returned %d", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
        MPI_Error_string(ierr, error_string, &size);
        MPITEST_message(MPITEST_NONFATAL, error_string);
    } else if (result != MPI_UNEQUAL) {
        fail++;
        sprintf(info_buf, "MPI_Comm_compare(NULL, SELF) result = %d, expected MPI_UNEQUAL", result);
        MPITEST_message(MPITEST_NONFATAL, info_buf);
    } 
#endif
    else pass++;

    /* comm1 */
    comm1 = MPI_COMM_NULL;
    /* comm2 */
    comm2 = MPI_COMM_NULL;

    ierr = MPI_Comm_compare(comm1, comm2, &result);
    if (ierr == MPI_SUCCESS) {
	fail++;
	sprintf(info_buf, "MPI_Comm_compare(NULL, NULL) returned success");
	MPITEST_message(MPITEST_NONFATAL, info_buf);
    }
#ifdef ORIGINAL_TEST
    if (ierr != MPI_SUCCESS) {
	fail++;
	sprintf(info_buf, "MPI_Comm_compare(NULL, NULL) returned %d", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
        MPI_Error_string(ierr, error_string, &size);
        MPITEST_message(MPITEST_NONFATAL, error_string);
    } else if (result != MPI_IDENT) {
        fail++;
        sprintf(info_buf, "MPI_Comm_compare(NULL, NULL) result = %d, expected MPI_IDENT", result);
        MPITEST_message(MPITEST_NONFATAL, info_buf);
    } 
#endif
    else pass++;

    /* report overall results  */
    MPITEST_report(pass, fail, 0, testname);

    ierr = MPI_Finalize();

    return fail;
}/* main() */
