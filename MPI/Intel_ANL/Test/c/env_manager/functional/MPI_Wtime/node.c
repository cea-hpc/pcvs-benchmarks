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
		  Test for MPI_Wtime and MPI_Wtick

******************************************************************************/

#include "mpitest_cfg.h"
#include "mpitest.h"

#if defined(HAVE_UNISTD_H)
#  include <unistd.h>
#endif

int main(int argc, char *argv[])
{
    int
	 fail,		/* Counts number of test failures  */
	 loop_cnt,	/* Counts number of tests executed */
	 verify,        /* Counts number of tests to verify*/
	 ierr,		/* Return value from MPI calls     */
	 i,
	 size;

    double
	time1,
	time2,
	tick;

    char 
	  info_buf[256],    /* buffer for passing mesages to MPITEST         */
	  testname[64];     /* the name of this test                         */
    char error_string[MPI_MAX_ERROR_STRING];

    /*-----------------------------  MPI_Init  ------------------------------*/
    ierr = MPI_Init(&argc, &argv);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr);
	MPITEST_message(MPITEST_FATAL, info_buf);
    }


    sprintf(testname, "MPI_Wtime");

    /*-----------------------------  MPITEST_init  --------------------------*/
    MPITEST_init(argc, argv);
    if (MPITEST_me == 0)
    {
	sprintf(info_buf, "Starting %s test", testname);
	MPITEST_message(MPITEST_INFO0, info_buf);
	MPITEST_message(MPITEST_VERIFY, "Verify value of MPI_Wtick below:");
    }

    /* set the global error counter */
    fail = 0;
    verify = 2;
    loop_cnt = 2;

    /*
    ** 
    */
    time1 = MPI_Wtime();
    tick = MPI_Wtick();

    for (i=0; i<MPITEST_nump; i++)
    {
	if (MPITEST_me == i)
	{
	    sprintf(info_buf, "MPI_Wtick() returned  %g seconds", tick);
	    MPITEST_message(MPITEST_VERIFY, info_buf);
	}
	MPI_Barrier(MPI_COMM_WORLD);
    }

    time2 = MPI_Wtime();

    if (time1 < 0)
    {
	sprintf(info_buf, "MPI_Wtime() returned a negative number %f", time1);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	fail++;
    }

    if (time2 <= time1)
    {
	sprintf(info_buf, "Successive calls to MPI_Wtime() returned %f, %f, expected incrementing values", time1, time2);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	fail++;
    }

    MPI_Barrier(MPI_COMM_WORLD);
    time1 = MPI_Wtime();
    sleep(5);
    time2 = MPI_Wtime();

    if (MPITEST_me == 0)
	MPITEST_message(MPITEST_VERIFY, "MPI_Wtime(); sleep(5); MPI_Wtime() returned:");

    for (i=0; i<MPITEST_nump; i++)
    {
	if (MPITEST_me == i)
	{
	    sprintf(info_buf, "%f seconds", time2-time1);
	    MPITEST_message(MPITEST_VERIFY, info_buf);
	}
	MPI_Barrier(MPI_COMM_WORLD);
    }

    /* report overall results  */

    MPITEST_report(loop_cnt - fail, fail, verify, testname);

    MPI_Finalize();

    return fail;

}/* main() */
