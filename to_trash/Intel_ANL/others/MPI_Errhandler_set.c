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
		  Test for MPI_Errhandler_create

MPI_COMM_WORLD is Dup'ed, and each are given a unique errorhandler.

An error is generated in both comms, plus one with MPI_COMM_NULL (which should
correspond to MPI_COMM_WORLD's error handler).  This test verifies each
handler is called correctly.
******************************************************************************/

#include "mpitest_cfg.h"
#include "mpitest.h"

    MPI_Comm
	comm;

    int
	 pass,		/* Counts number of tests passed   */
	 fail,		/* Counts number of test failures  */
	 count1,
	 count2;

void err1(MPI_Comm *commin, int *code, ...)
{ /* Handles errors for MPI_COMM_WORLD */
    int  class;

    if (count1 == 0)
    { /* Bad length on MPI_Send */
	MPI_Error_class(*code, &class);
	if (class != MPI_ERR_COUNT)
	{
	    MPITEST_message(MPITEST_NONFATAL, "First error returned incorrect code");
	} else pass++;
    }

    else if (count1 == 1)
    { /* Bad communicator in MPI_Barrier */
	MPI_Error_class(*code, &class);
	if (class != MPI_ERR_COMM)
	{
	    MPITEST_message(MPITEST_NONFATAL, "Second error returned incorrect code");
	    fail++;
	} else pass++;
    }

    else
    {
	MPITEST_message(MPITEST_NONFATAL, "Errhandler 1 called too many times");
	fail++;
    }
    count1++; 
}

void err2(MPI_Comm *commin, int *code, ...)
{ /* Handles errors for Dup'ed comm */
    int  class;

    if (count2 == 0)
    { /* Bad rank on MPI_Send */
	MPI_Error_class(*code, &class);
	if (class != MPI_ERR_RANK)
	{
	    MPITEST_message(MPITEST_NONFATAL, "Third error returned incorrect code");
	} else pass++;
    }
    else
    {
	MPITEST_message(MPITEST_NONFATAL, "Errhandler 2 called too many times");
	fail++;
    }
    count2++; 
}

int main(int argc, char *argv[])
{
    int
	 ierr,		/* Return value from MPI calls     */
	 size;

    char 
	  info_buf[256],    /* buffer for passing mesages to MPITEST         */
	  testname[64];     /* the name of this test                         */
    char error_string[MPI_MAX_ERROR_STRING];

    MPI_Errhandler
	errh1,
	errh2;

    /*-----------------------------  MPI_Init  ------------------------------*/
    ierr = MPI_Init(&argc, &argv);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr);
	MPITEST_message(MPITEST_FATAL, info_buf);
    }

    sprintf(testname, "MPI_Errhandler_set");
    pass = 0;
    fail = 0;
    count1 = 0;
    count2 = 0;

    /*-----------------------------  MPITEST_init  --------------------------*/
    MPITEST_init(argc, argv);
    if (MPITEST_me == 0)
    {
	sprintf(info_buf, "Starting %s test", testname);
	MPITEST_message(MPITEST_INFO0, info_buf);
    }

    /* Create a second communicator */
    ierr = MPI_Comm_dup(MPI_COMM_WORLD, &comm);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_dup", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_FATAL, error_string);
    }

    /* Create 2 errorhandlers */
    ierr = MPI_Errhandler_create(err1, &errh1);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Errhandler_create #1", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_FATAL, error_string);
    }
    pass++;

    ierr = MPI_Errhandler_create(err2, &errh2);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Errhandler_create #2", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_FATAL, error_string);
    }
    pass++;

    /* Set the 2 errorhandlers */
    ierr = MPI_Errhandler_set(MPI_COMM_WORLD, errh1);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Errhandler_set #1", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_FATAL, error_string);
    }
    pass++;

    ierr = MPI_Errhandler_set(comm, errh2);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Errhandler_set #2", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_FATAL, error_string);
    }
    pass++;


    /* Now create some errors and verify that they work */
    /* COMM_WORLD */
    MPI_Send(&ierr, -1, MPI_INT, 0, 0, MPI_COMM_WORLD);

    /* This should also be handled by MPI_COMM_WORLD */
    MPI_Barrier(MPI_COMM_NULL);

    /* Dup'ed comm */
    MPI_Send(&ierr, 1, MPI_INT, MPITEST_nump, 0, comm);

    if (count1 != 2)
    {
	MPITEST_message(MPITEST_NONFATAL, "Errhandler 1 was not called correctly when an error occurred");
	fail++;
    }

    if (count2 != 1)
    {
	MPITEST_message(MPITEST_NONFATAL, "Errhandler 2 was not called correctly when an error occurred");
	fail++;
    }

    /* report overall results  */
    MPITEST_report(pass, fail, 0, testname);

    MPI_Finalize();

    return fail;

}/* main() */
