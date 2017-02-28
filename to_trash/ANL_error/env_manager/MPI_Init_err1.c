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
		  Error Test for MPI_Init()

This test calls MPI_Init() twice; expecting an error on the second call.
Since this does not fall cleanly into any MPI Error class, it expects
to receive MPI_ERR_OTHER.

******************************************************************************/

#include "mpitest_cfg.h"
#include "mpitest.h"

int main(int argc, char *argv[])
{
    int
	 fail,		/* Counts number of test failures  */
	 pass,    	/* Counts number of tests executed */
	 ierr,		/* Return value from MPI calls     */
	 len,		/* Length of String		   */
	 errorclass,	/* Error class of ierr             */
	 size,
	 flag;

    char 
	  info_buf[256],    /* buffer for passing mesages to MPITEST         */
	  testname[64];     /* the name of this test                         */
    char error_string[MPI_MAX_ERROR_STRING];

    /*-----------------------------  MPI_Init  ------------------------------*/
    ierr = MPI_Init(&argc, &argv);
    info_buf[0]='\0';
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr);
	MPITEST_message(MPITEST_FATAL, info_buf);
    }


    sprintf(testname, "MPI_Init_err1");

    /*-----------------------------  MPITEST_init  --------------------------*/
    MPITEST_init(argc, argv);
    if (MPITEST_me == 0)
    {
	sprintf(info_buf, "Starting %s test", testname);
	MPITEST_message(MPITEST_INFO0, info_buf);
    }

    /* set the global error counter */
    fail = 0;
    pass = 0;


    /*
    **  Set an errorhandler so we get control back.
    */
    ierr = MPI_Errhandler_set(MPI_COMM_WORLD,MPI_ERRORS_RETURN);
    if (ierr != MPI_SUCCESS)
    {
	fail++;
	sprintf(info_buf, "MPI_Errorhandler_set returned %d", ierr);
	MPITEST_message(MPITEST_FATAL, info_buf);
    }

    /*
    ** Second call to MPI_Init()
    */

    ierr = MPI_Init(&argc, &argv);
    if (ierr == MPI_SUCCESS)
    {
        fail++;
        sprintf(info_buf, "Second MPI_Init() call did not FAIL");
        MPITEST_message(MPITEST_NONFATAL, info_buf);
    }
    else
    {
        MPI_Error_class(ierr, &errorclass);
        if (errorclass != MPI_ERR_OTHER)
        {
	    fail++;
            sprintf(info_buf, "Second MPI_Init() call returned %d, expected MPI_ERR_OTHER",
              errorclass);
            MPITEST_message(MPITEST_NONFATAL, info_buf);
            MPI_Error_string(ierr, error_string, &size);
            MPITEST_message( MPITEST_NONFATAL, info_buf);
        }
        else
        {
            pass++;
            MPI_Error_string(ierr, error_string, &size);
            MPITEST_message( MPITEST_INFO0, info_buf);
        }
    }


    /* report overall results  */

    MPITEST_report(pass, fail, 0, testname);

    MPI_Finalize();

    return fail;

}/* main() */
