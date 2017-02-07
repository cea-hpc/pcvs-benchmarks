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
		  Test for MPI_Set_errhandler(MPI_ERRORS_ARE_FATAL)

The Errhandler for MPI_COMM_WORLD is set to MPI_ERRORS_RETURN and is dup'ed.

The MPI errorhandler on the dup'ed communicator is set to MPI_ERRORS_ARE_FATAL,
then an illegal call is made.  If the call returns, the test is considered a
failure.
******************************************************************************/

#include "mpitest_cfg.h"
#include "mpitest.h"

int main(int argc, char *argv[])
{
    int
	 fail,		/* Counts number of test failures  */
	 ierr,		/* Return value from MPI calls     */
	 size;

    char 
	  info_buf[256],    /* buffer for passing mesages to MPITEST         */
	  testname[64];     /* the name of this test                         */
    char error_string[MPI_MAX_ERROR_STRING];

    MPI_Comm
	comm;

    MPC_EXIT_ON_ABORT();
    /*-----------------------------  MPI_Init  ------------------------------*/
    ierr = MPI_Init(&argc, &argv);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr);
	MPITEST_message(MPITEST_FATAL, info_buf);
    }


    sprintf(testname, "MPI_Errhandler_fatal");

    /*-----------------------------  MPITEST_init  --------------------------*/
    fail = 0;
    MPITEST_init(argc, argv);
    if (MPITEST_me == 0)
    {
	sprintf(info_buf, "Starting %s test", testname);
	MPITEST_message(MPITEST_INFO0, info_buf);
	MPITEST_message(MPITEST_INFO0, "This test will abort after printing the results message");
	MPITEST_message(MPITEST_INFO0, "If it does not, then a f.a.i.l.u.r.e will be noted");
    }

    ierr = MPI_Errhandler_set(MPI_COMM_WORLD, MPI_ERRORS_RETURN);
    if (ierr != MPI_SUCCESS)
    {
        fail++;
        sprintf(info_buf, "MPI_Errorhandler_set #1 returned %d", ierr);
        MPITEST_message(MPITEST_FATAL, info_buf);
    }

    ierr = MPI_Comm_dup(MPI_COMM_WORLD, &comm);
    if (ierr != MPI_SUCCESS)
    {
        fail++;
        sprintf(info_buf, "MPI_Comm_dup returned %d", ierr);
        MPITEST_message(MPITEST_FATAL, info_buf);
    }

    ierr = MPI_Errhandler_set(comm, MPI_ERRORS_ARE_FATAL);
    if (ierr != MPI_SUCCESS)
    {
        fail++;
        sprintf(info_buf, "MPI_Errorhandler_set #2 returned %d", ierr);
        MPITEST_message(MPITEST_FATAL, info_buf);
    }

    /* report overall results  */
    MPITEST_report(1, fail, 0, testname);
    /* Make sure that the output lines are flushed */
    fflush(stdout);
    fflush(stderr);
    /* Make sure that the flushes complete before we trigger the error */
    MPI_Barrier( comm );

    ierr = MPI_Send(&ierr, 1, MPI_INT, MPITEST_nump, 0, comm);
    sprintf(info_buf, "MPI_Barrier(MPI_COMM_NULL) ierr = %d, should have ABORTED!", ierr);
    MPITEST_message(MPITEST_NONFATAL, info_buf);
    fail++;
    MPITEST_report(1, fail, 0, testname);

    MPI_Finalize();

    return fail;

}/* main() */
