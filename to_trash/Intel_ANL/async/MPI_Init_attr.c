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
		  Test for pre-defined MPI attributes.

This test calls MPI_Attr_get() on each rank for the following
pre-defined attributes:
  MPI_TAG_UB
  MPI_HOST
  MPI_WTIME_IS_GLOBAL
  MPI_IO
and prints the result.

You must manually verify that the attribute is correct for each rank.
******************************************************************************/

#include "mpitest_cfg.h"
#include "mpitest.h"

int main(int argc, char *argv[])
{
    int
	 fail,		/* Counts number of test failures  */
	 loop_cnt,	/* Counts number of tests executed */
	 verify,        /* Counts number of tests to verify*/
	 ierr,		/* Return value from MPI calls     */
	*attr,		/* Attribute returned		   */
	 found,		/* attribute exists		   */
	 i,
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


    sprintf(testname, "MPI_Init_attr");

    /*-----------------------------  MPITEST_init  --------------------------*/
    MPITEST_init(argc, argv);
    if (MPITEST_me == 0)
    {
	sprintf(info_buf, "Starting %s test", testname);
	MPITEST_message(MPITEST_INFO0, info_buf);
	MPITEST_message(MPITEST_VERIFY, "Verify values of the following environmental attributes:");
    }

    /* set the global error counter */
    fail = 0;
    verify = 0;
    loop_cnt = 4;

    /*
    ** MPI_TAG_UB
    */
    ierr = MPI_Attr_get(MPI_COMM_WORLD, MPI_TAG_UB, &attr, &found);
    for (i=0; i<MPITEST_nump; i++)
    {
	if (i == MPITEST_me)
	{
            if (ierr != MPI_SUCCESS)
            {
                sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_get(MPI_TAG_UB)", ierr);
                MPITEST_message(MPITEST_NONFATAL, info_buf);
                MPI_Error_string(ierr, error_string, &size);
        	MPITEST_message(MPITEST_NONFATAL, error_string);
        	fail++;
	    }       /* Error Test  */
	    else if (found == 0)
	    {
		fail++;
		sprintf(info_buf, "MPI_Attr_get reported MPI_TAG_UB not found");
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	    }
	    else
	    {
		verify++;
		sprintf(info_buf, "MPI_TAG_UB=%d", *attr);
		MPITEST_message(MPITEST_VERIFY, info_buf);
	    }
	}
	MPI_Barrier(MPI_COMM_WORLD);
    }

    /*
    ** MPI_HOST
    */
    ierr = MPI_Attr_get(MPI_COMM_WORLD, MPI_HOST, &attr, &found);
    for (i=0; i<MPITEST_nump; i++)
    {
	if (i == MPITEST_me)
	{
            if (ierr != MPI_SUCCESS)
            {
                sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_get(MPI_HOST)", ierr);
                MPITEST_message(MPITEST_NONFATAL, info_buf);
                MPI_Error_string(ierr, error_string, &size);
        	MPITEST_message(MPITEST_NONFATAL, error_string);
        	fail++;
	    }       /* Error Test  */
	    else if (found == 0)
	    {
		fail++;
		sprintf(info_buf, "MPI_Attr_get reported MPI_HOST not found");
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	    }
	    else
	    {
		verify++;
		if (*attr == MPI_PROC_NULL)
		    sprintf(info_buf, "MPI_HOST=MPI_PROC_NULL");
		else
		    sprintf(info_buf, "MPI_HOST=%d", *attr);
		MPITEST_message(MPITEST_VERIFY, info_buf);
	    }
	}
	MPI_Barrier(MPI_COMM_WORLD);
    }

    /*
    ** MPI_WTIME_IS_GLOBAL
    */
    ierr = MPI_Attr_get(MPI_COMM_WORLD, MPI_WTIME_IS_GLOBAL, &attr, &found);
    for (i=0; i<MPITEST_nump; i++)
    {
	if (i == MPITEST_me)
	{
            if (ierr != MPI_SUCCESS)
            {
                sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_get(MPI_WTIME_IS_GLOBAL)", ierr);
                MPITEST_message(MPITEST_NONFATAL, info_buf);
                MPI_Error_string(ierr, error_string, &size);
        	MPITEST_message(MPITEST_NONFATAL, error_string);
        	fail++;
	    }       /* Error Test  */
	    else if (found == 0)
	    {
		verify++;
		sprintf(info_buf, "MPI_Attr_get reported MPI_WTIME_IS_GLOBAL not found");
		MPITEST_message(MPITEST_VERIFY, info_buf);
	    }
	    else
	    {
		verify++;
		sprintf(info_buf, "MPI_WTIME_IS_GLOBAL=%d (1=synchronized, else 0)", *attr);
		MPITEST_message(MPITEST_VERIFY, info_buf);
	    }
	}
	MPI_Barrier(MPI_COMM_WORLD);
    }

    /*
    ** MPI_IO
    */
    ierr = MPI_Attr_get(MPI_COMM_WORLD, MPI_IO, &attr, &found);
    for (i=0; i<MPITEST_nump; i++)
    {
	if (i == MPITEST_me)
	{
            if (ierr != MPI_SUCCESS)
            {
                sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_get(MPI_IO)", ierr);
                MPITEST_message(MPITEST_NONFATAL, info_buf);
                MPI_Error_string(ierr, error_string, &size);
        	MPITEST_message(MPITEST_NONFATAL, error_string);
        	fail++;
	    }       /* Error Test  */
	    else if (found == 0)
	    {
		fail++;
		sprintf(info_buf, "MPI_Attr_get reported MPI_IO not found");
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	    }
	    else
	    {
		verify++;
		if (*attr == MPI_ANY_SOURCE)
		    sprintf(info_buf, "MPI_IO=MPI_ANY_SOURCE");
		else
		    sprintf(info_buf, "MPI_IO=%d", *attr);
		MPITEST_message(MPITEST_VERIFY, info_buf);
	    }
	}
	MPI_Barrier(MPI_COMM_WORLD);
    }


    /* report overall results  */

    MPITEST_report(loop_cnt - fail, fail, verify, testname);

    MPI_Finalize();

    return fail;

}/* main() */
