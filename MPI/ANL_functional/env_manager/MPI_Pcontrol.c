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
		  Test for MPI_Pcontrol()

This test calls MPI_Pcontrol(), with various values.
******************************************************************************/

#include "mpitest_cfg.h"
#include "mpitest.h"

int main(int argc, char *argv[])
{
    int
	 ierr,		/* Return value from MPI calls     */
	 pass,
	 fail,
	 size;
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


    sprintf(testname, "MPI_Pcontrol");

    /*-----------------------------  MPITEST_init  --------------------------*/
    MPITEST_init(argc, argv);
    if (MPITEST_me == 0)
    {
	sprintf(info_buf, "Starting %s test", testname);
	MPITEST_message(MPITEST_INFO0, info_buf);
    }

    pass = 0;
    fail = 0;

    ierr = MPI_Pcontrol(0);
    if (ierr != MPI_SUCCESS)
    {
        sprintf(info_buf, "Non-zero return code (%d) from MPI_Pcontrol(0)", ierr);
        MPITEST_message(MPITEST_NONFATAL, info_buf);
        MPI_Error_string(ierr, error_string, &size);
        MPITEST_message(MPITEST_NONFATAL, error_string);
        fail++;
    }
    else
	pass++;

    ierr = MPI_Pcontrol(1);
    if (ierr != MPI_SUCCESS)
    {
        sprintf(info_buf, "Non-zero return code (%d) from MPI_Pcontrol(1)", ierr);
        MPITEST_message(MPITEST_NONFATAL, info_buf);
        MPI_Error_string(ierr, error_string, &size);
        MPITEST_message(MPITEST_NONFATAL, error_string);
        fail++;
    }
    else
	pass++;

    ierr = MPI_Pcontrol(2);
    if (ierr != MPI_SUCCESS)
    {
        sprintf(info_buf, "Non-zero return code (%d) from MPI_Pcontrol(2)", ierr);
        MPITEST_message(MPITEST_NONFATAL, info_buf);
        MPI_Error_string(ierr, error_string, &size);
        MPITEST_message(MPITEST_NONFATAL, error_string);
        fail++;
    }
    else
	pass++;

    ierr = MPI_Pcontrol(1, 0, 4);
    if (ierr != MPI_SUCCESS)
    {
        sprintf(info_buf, "Non-zero return code (%d) from MPI_Pcontrol(1, 0, 4)", ierr);
        MPITEST_message(MPITEST_NONFATAL, info_buf);
        MPI_Error_string(ierr, error_string, &size);
        MPITEST_message(MPITEST_NONFATAL, error_string);
        fail++;
    }
    else
	pass++;

    /* report overall results  */

    MPITEST_report(pass, fail, 0, testname);

    MPI_Finalize();

    return fail;

}/* main() */
