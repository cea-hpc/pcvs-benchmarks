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
*                          Error test for MPI_Startall
*
* This test verifies that the correct error is returned if MPI_Startall
* called with an invalid argument.
*
*  MPI_Startall error tests
*  --------------------------------------------------------
* 1)  Request already active ............................ [MPI_ERR_REQUEST]
* 2)  Freed request ..................................... [MPI_ERR_REQUEST]
*-------------------------------------------------------------------------
*
* In all cases, expect to receive appropriate error.
*
* Test history:
*    1  04/12/96     gt   Created
*
******************************************************************************/
#include "mpitest_cfg.h"
#include "mpitest.h"

int main(int argc, char *argv[])

{
    int
        dest,	        /* Destination of Ssend_init message                 */
        pass, fail,	/* counts total number # of failures                 */
        verify,    	/* counts total number of verify failures            */
        ierr,	        /* return value from MPI calls                       */
        errorclass,	/* error class of ierr                               */
        size;           /* length of error message                           */

    int
        input[40],	/* input to Ssend_init                               */
        output[40];	/* receive buffer                                    */

    char
        testname[128],	/* the name of this test                             */
        info_buf[256];	/* for sprintf                                       */
    char error_string[MPI_MAX_ERROR_STRING];

    MPI_Request
        request;        /*  MPI request structure                            */

    MPI_Status
	stat;           /*  MPI status  structure                            */
    /*-----------------------------------------------------------------------*/

    /*   
    **  Initialize the MPI environment and test environment.
    */

    ierr = MPI_Init(&argc, &argv);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr);
	MPITEST_message(MPITEST_FATAL, info_buf);
    }

    sprintf(testname, "MPI_Startall_err1:  Request already active");
    MPITEST_init(argc, argv);
    if (MPITEST_me == 0)
    {
	sprintf(info_buf, "Starting %s test", testname);
	MPITEST_message(MPITEST_INFO0, info_buf);
    }
    pass = 0;
    fail = 0;
    verify = 0;

    /*
     * *  Set an errorhandler so we get control back.
     */

    ierr = MPI_Errhandler_set(MPI_COMM_WORLD, MPI_ERRORS_RETURN);

    if (ierr != MPI_SUCCESS)
    {
	fail++;
	sprintf(info_buf, "MPI_Errorhandler_set returned %d", ierr);
	MPITEST_message(MPITEST_FATAL, info_buf);
    }

    /* We need 2 nodes to test this */
    if (MPITEST_nump < 2)
    {
	sprintf(info_buf, "Not enough ranks to test MPI_Startall on already active request");
	MPITEST_message(MPITEST_FATAL, info_buf);
    }


    /*
     * *  Make a valid request (Ssend to self)
     */
    sprintf(info_buf, "about to call MPI_Ssend_init ");
    MPITEST_message(MPITEST_INFO1, info_buf);

    ierr = 0;

    /*-------------------------------  Ssend_init-------------------------------*/

    if (MPITEST_me == 0)
    {
	dest = 1;
	ierr = MPI_Ssend_init(input, 20, MPI_INT, dest, 0, MPI_COMM_WORLD, &request);

	if (ierr != MPI_SUCCESS)
	{
	    sprintf(info_buf, "Valid MPI_Ssend_init returned %d", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_FATAL, error_string);
	}

	/*
	 *  Do a valid startall; expect success.
	*/
	ierr = MPI_Startall(1, &request);
	if (ierr != MPI_SUCCESS)
	{
	    sprintf(info_buf, "Valid MPI_Startall returned %d", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_FATAL, error_string);
	}

	/*
	 *  Do another start; expect failure.
	*/
	ierr = MPI_Startall(1, &request);
	if (ierr == MPI_SUCCESS)
	{
	    fail++;
	    sprintf(info_buf, "MPI_Startall on already active request did not FAIL");
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	}
	else
	{
	    MPI_Error_class(ierr, &errorclass);
	    if (errorclass != MPI_ERR_REQUEST)
	    {
		fail++;
		sprintf(info_buf, "MPI_Startall on already active request returned %d, expected MPI_ERR_REQUEST",
			errorclass);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
	    }
	    else
	    {
		pass++;
		sprintf(info_buf, "ierr = %d, errorclass = %d", ierr,
			errorclass);
		MPITEST_message(MPITEST_INFO2, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_INFO1, error_string);
	    }
	}
    }

    ierr = MPI_Barrier(MPI_COMM_WORLD);

    if (MPITEST_me == 1)
    {

	/*
	 *  Complete the first request.
	*/
	dest = 0;
	ierr = MPI_Recv(output, 20, MPI_INT, dest, 0, MPI_COMM_WORLD, &stat);
	if (ierr != MPI_SUCCESS)
	{
	    fail++;
	    sprintf(info_buf, "MPI_Recv on valid Ssend_init Start returned %d", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_NONFATAL, error_string);
	}
    }


    if (MPITEST_me == 0)
    {
	ierr = MPI_Request_free(&request);
	if (ierr != MPI_SUCCESS)
	{
	    fail++;
	    sprintf(info_buf, "MPI_Request_free returned %d", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_NONFATAL, error_string);
	}
    }

    /* report overall results  */
    MPITEST_report(pass, fail, verify, testname);

    MPI_Finalize();
    return fail;
}/* main() */
