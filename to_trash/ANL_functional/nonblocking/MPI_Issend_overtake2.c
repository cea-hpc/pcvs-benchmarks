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
		  Test for MPI_Issend() 

This test verifies that the basic blocking MPI_Ssend operation makes progress
when matched by an MPI_Irecv, even though the MPI_Wait does not occur until
later (taken from Example 3.14 from the MPI Spec).

Rank 0 does an Ssend followed by a Send, Rank 1 does an Irecv followed by
a Recv, then the Wait for the first Irecv.  According to the Spec., this
should work.

******************************************************************************/

#include "mpitest_cfg.h"
#include "mpitest.h"

int main(int argc, char *argv[])
{

    int
        loop_cnt,           /* counts total number of times through loop     */
        fail,   	    /* counts total number of failures               */
        ierr,	            /* return value from MPI calls                   */
        size,	            /* return size from MPI_Error_string             */
	i,
	flag;

	char buff1[3];     /* buffer for first message                       */
	char buff2[4];     /* buffer for second message                      */

        char
	  info_buf[256],    /* buffer for passing mesages to MPITEST         */
	  testname[64];     /* the name of this test                         */
    char error_string[MPI_MAX_ERROR_STRING];

        MPI_Status
	  recv1_stat,
	  recv2_stat;

        MPI_Request
	  request1,
	  request2;
    /*-----------------------------  MPI_Init  ------------------------------*/
    ierr = MPI_Init(&argc, &argv);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr);
	MPITEST_message(MPITEST_FATAL, info_buf);
    }


    sprintf(testname, "MPI_Issend_overtake2");

    /*-----------------------------  MPITEST_init  --------------------------*/
    MPITEST_init(argc, argv);
    if (MPITEST_me == 0)
    {
	sprintf(info_buf, "Starting %s test", testname);
	MPITEST_message(MPITEST_INFO0, info_buf);
    }

    if (MPITEST_nump < 2)
    {
	sprintf(info_buf, "At least 2 ranks required to run this test");
	MPITEST_message(MPITEST_FATAL, info_buf);
    }

    /* set the global error counter */
    loop_cnt = 0;
    fail = 0;

    if (MPITEST_me < 2)
    {

	if (MPITEST_me == 0)
	{ /* sender */
	    loop_cnt = 1;
	    MPI_Barrier(MPI_COMM_WORLD);

	    ierr = MPI_Ssend(&buff1, 
			      3, 
			      MPI_CHAR, 
			      1, 
			      1, 
			      MPI_COMM_WORLD);

	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Ssend #1", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail = 1;
	    }	/* Ssend Error Test  */


	    ierr = MPI_Send(&buff2, 
			      4, 
			      MPI_CHAR, 
			      1, 
			      2, 
			      MPI_COMM_WORLD);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Send #2", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail = 1;
	    }	/*  Ssend Error Test  */


	} /* sender */
	else
	{ /* receiver */
	    loop_cnt = 1;
	    MPI_Barrier(MPI_COMM_WORLD);

	    ierr = MPI_Irecv(&buff1, 3, MPI_CHAR, 0, 1, MPI_COMM_WORLD,
			&request2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Recv", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail = 1;
	    }	/* Error Test  */

	    ierr = MPI_Recv(&buff2, 4, MPI_CHAR, 0, 2, MPI_COMM_WORLD,
			&recv1_stat);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Irecv", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail = 1;
	    }
	    ierr =  MPI_Wait(&request2, &recv2_stat);

	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Wait on Irecv 2", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	        MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail = 1;
	     }  /* Wait on Ssend Error Test  */

	} /* receiver */

    }
    else
    {   /* rank >= 2 need to match Barrier above */
	MPI_Barrier(MPI_COMM_WORLD);
    }

    /* report overall results  */

    MPITEST_report(loop_cnt - fail, fail, 0, testname);

    MPI_Finalize();

    return fail;

}/* main() */
