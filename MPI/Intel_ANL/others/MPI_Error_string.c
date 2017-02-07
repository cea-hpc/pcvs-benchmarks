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
		  Test for MPI_Error_string()

This test calls MPI_Error_string() for each valid error class and
prints it.  You must manually verify that the error string is indeed
appropriate for the error being tested.

******************************************************************************/

#include "mpitest_cfg.h"
#include "mpitest.h"

void test_error(int error, char *errstr, int *fail)
{
    int
	 ierr,		/* Return value from MPI calls     */
	 len,		/* Length of String		   */
	 size;

    char 
	  string[MPI_MAX_ERROR_STRING],
	  info_buf[256];    /* buffer for passing mesages to MPITEST         */
    char error_string[MPI_MAX_ERROR_STRING];
	
	MPITEST_message(MPITEST_VERIFY, errstr);
	ierr = MPI_Error_string(error, string, &len);
	if (ierr != MPI_SUCCESS)
	{
	    sprintf(info_buf, "Non-zero return code (%d) from MPI_Error_string", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_NONFATAL, error_string);
	    *fail = *fail + 1;
	}       /* Error Test  */
	else
	{
	    if ((len > MPI_MAX_ERROR_STRING) || (len != strlen(string)))
	    {
	        *fail = *fail + 1;
	        sprintf(info_buf, "Returned length=%d, MPI_MAX_ERROR_STRING=%d",
			len, MPI_MAX_PROCESSOR_NAME);
	        MPITEST_message(MPITEST_NONFATAL, info_buf);
	        MPITEST_message(MPITEST_INFO0, string);
	    }
	    else
	    {
	        sprintf(info_buf, "Returned string length=%d (OK), string:", len);
	        MPITEST_message(MPITEST_VERIFY, info_buf);
	        MPITEST_message(MPITEST_VERIFY, string);
	    }
	}
}


int main(int argc, char *argv[])
{
    int
	 fail,		/* Counts number of test failures  */
	 loop_cnt,	/* Counts number of tests executed */
	 ierr,		/* Return value from MPI calls     */
	 len;		/* Length of String		   */

    char 
	  info_buf[256],    /* buffer for passing mesages to MPITEST         */
	  testname[64];     /* the name of this test                         */

    /*-----------------------------  MPI_Init  ------------------------------*/
    ierr = MPI_Init(&argc, &argv);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr);
	MPITEST_message(MPITEST_FATAL, info_buf);
    }


    sprintf(testname, "MPI_Error_string");

    /*-----------------------------  MPITEST_init  --------------------------*/
    MPITEST_init(argc, argv);
    if (MPITEST_me == 0)
    {
	sprintf(info_buf, "Starting %s test", testname);
	MPITEST_message(MPITEST_INFO0, info_buf);
	MPITEST_message(MPITEST_VERIFY, "Please verify the following error strings");
    }

    /* set the global error counter */
    fail = 0;
    loop_cnt = 0;

    if (MPITEST_me != 0)
    {
	MPITEST_report(0, 0, 0, testname);
    }
    else
    {
	test_error(MPI_SUCCESS, "MPI_SUCCESS", &fail);
	loop_cnt++;
	test_error(MPI_ERR_BUFFER, "MPI_ERR_BUFFER", &fail);
	loop_cnt++;
	test_error(MPI_ERR_COUNT, "MPI_ERR_COUNT", &fail);
	loop_cnt++;
	test_error(MPI_ERR_TYPE, "MPI_ERR_TYPE", &fail);
	loop_cnt++;
	test_error(MPI_ERR_TAG, "MPI_ERR_TAG", &fail);
	loop_cnt++;
	test_error(MPI_ERR_COMM, "MPI_ERR_COMM", &fail);
	loop_cnt++;
	test_error(MPI_ERR_RANK, "MPI_ERR_RANK", &fail);
	loop_cnt++;
	test_error(MPI_ERR_REQUEST, "MPI_ERR_REQUEST", &fail);
	loop_cnt++;
	test_error(MPI_ERR_ROOT, "MPI_ERR_ROOT", &fail);
	loop_cnt++;
	test_error(MPI_ERR_GROUP, "MPI_ERR_GROUP", &fail);
	loop_cnt++;
	test_error(MPI_ERR_OP, "MPI_ERR_OP", &fail);
	loop_cnt++;
	test_error(MPI_ERR_TOPOLOGY, "MPI_ERR_TOPOLOGY", &fail);
	loop_cnt++;
	test_error(MPI_ERR_DIMS, "MPI_ERR_DIMS", &fail);
	loop_cnt++;
	test_error(MPI_ERR_UNKNOWN, "MPI_ERR_UNKNOWN", &fail);
	loop_cnt++;
	test_error(MPI_ERR_TRUNCATE, "MPI_ERR_TRUNCATE", &fail);
	loop_cnt++;
	test_error(MPI_ERR_OTHER, "MPI_ERR_OTHER", &fail);
	loop_cnt++;
	test_error(MPI_ERR_INTERN, "MPI_ERR_INTERN", &fail);
	loop_cnt++;
	test_error(MPI_ERR_IN_STATUS, "MPI_ERR_IN_STATUS", &fail);
	loop_cnt++;
	test_error(MPI_ERR_PENDING, "MPI_ERR_PENDING", &fail);
	loop_cnt++;
	/* report overall results  */

	MPITEST_report(loop_cnt - fail, fail, loop_cnt - fail, testname);
    }

    MPI_Finalize();

    return fail;

}/* main() */
