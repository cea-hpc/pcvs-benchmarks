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
                          Test for MPI_collective_ovlp

This test creates two overlapping communicators then tests (correctly)
overlapping collective calls using these communicators.  This is
something that may happen (for example) with a library, where a subset
of ranks are doing a collective operation, while a few other nodes are
also doing a collective operation on a different communicator, waiting
for the nodes in the library to catch up.

There should be no interference from the multiple calls.  Each communicator
uses unique data, so Incorrect data received or an MPI error constitutes
failure.

Test history:
   1  09/03/96     gt       Original version

******************************************************************************/

#include "mpitest_cfg.h"
#include "mpitest.h"

#if defined(HAVE_UNISTD_H)
#  include <unistd.h>
#endif

int main(int argc, char *argv[])
{
    int
	 fail,          /* Counts number of test failures  */
	 loop_cnt,      /* Counts number of tests executed */
	 loop_fail,
	 ierr,          /* Return value from MPI calls     */
	 rank2, 	/* Rank in comm 2                  */
	 rank3,         /* Rank in comm 3                  */
	 size,
	 i,		/* iterative value (rank to excl)  */
	 j,		/* iterative value (rank to excl)  */
	 k,		/* iterative value                 */
	 value,		/* input to collective call        */
	*value_ary,	/* input to collective call        */
	*value_dsplsary,/* input to collective call        */
	*counts_ary,	/* input to collective call        */
	 expect,	/* expected result                 */
	 result,	/* result of collective call       */
	*result_ary,	/* result of collective call       */
	*result_dsplsary;/* result of collective call      */

    MPI_Comm
	 comm2,		/* Communicator to test            */
	 comm3;		/* Overlapping communicator to test*/

    MPI_Group
	 group,		/* Group of MPI_COMM_WORLD         */
	 group2,        /* Group of comm2                  */
	 group3;        /* Group of comm3                  */

    char
	  info_buf[256],/* buffer for passing mesages to MPITEST  */
	  testname[64]; /* the name of this test           */
    char error_string[MPI_MAX_ERROR_STRING];

    /*
     * Initialize MPI
     */
    ierr = MPI_Init (&argc, &argv);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr);
	MPITEST_message(MPITEST_FATAL, info_buf);
    }


    sprintf(testname, "MPI_collective_overlap");

    /*-----------------------------  MPITEST_init  --------------------------*/
    MPITEST_init(argc, argv);
    if (MPITEST_me == 0)
    {
	sprintf(info_buf, "Starting %s test", testname);
	MPITEST_message(MPITEST_INFO0, info_buf);
    }

    if (MPITEST_nump <= 2)
    {
	MPITEST_message(MPITEST_FATAL, "This test requires at least 3 ranks to run");
    }

    /* set the global error counter */
    fail = 0;
    loop_cnt = 0;

    /*
     * Allocate memory for the various collective structures
    */
    value_ary = (int *)malloc(MPITEST_nump * sizeof(int));
    value_dsplsary = (int *)malloc(MPITEST_nump * sizeof(int));
    result_ary = (int *)malloc(MPITEST_nump * sizeof(int));
    result_dsplsary = (int *)malloc(MPITEST_nump * sizeof(int));
    counts_ary = (int *)malloc(MPITEST_nump * sizeof(int));
    if ((value_ary == 0)  || (result_ary == 0) ||
	(value_dsplsary == 0) || (result_dsplsary == 0) ||
	(counts_ary == 0))
    {
	MPITEST_message(MPITEST_FATAL, "Unable to malloc memory for this test");
    }

    for (k=0; k<MPITEST_nump; k++)
    {
	value_dsplsary[k] = k;
	result_dsplsary[k] = k;
	counts_ary[k] = 1;
	
    }


    /*
     * Create a group from MPI_COMM_WORLD
    */
    ierr =  MPI_Comm_group(MPI_COMM_WORLD, &group);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_group", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_FATAL, error_string);
	fail++;
    }       /* Error Test  */




    for (i=0; (i<6) && (i<MPITEST_nump); i++)   /* Loop through ranks excluded */
    {

	/*
	 * Exclude one rank to create group2, make a communicator, and
	 * determine my rank in the new communicator.
	*/
	ierr =  MPI_Group_excl(group, 1, &i, &group2);
	if (ierr != MPI_SUCCESS)
	{
	    sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_excl #1", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_FATAL, error_string);
	    fail++;
	}       /* Error Test  */

	ierr =  MPI_Comm_create(MPI_COMM_WORLD, group2, &comm2);
	if (ierr != MPI_SUCCESS)
	{
	    sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_create #1", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_FATAL, error_string);
	    fail++;
	}       /* Error Test  */

	ierr =  MPI_Group_rank(group2, &rank2);
	if (ierr != MPI_SUCCESS)
	{
	    sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_rank #1", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_FATAL, error_string);
	    fail++;
	}       /* Error Test  */



	/*
	 * Exclude a different rank to create group3 to make an overlapping 
	 * communicator, and determine my rank in the new communicator.
	*/
	j = MPITEST_nump - i - 1;
	ierr =  MPI_Group_excl(group, 1, &j, &group3);
	if (ierr != MPI_SUCCESS)
	{
	    sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_excl #2", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_FATAL, error_string);
	    fail++;
	}       /* Error Test  */

	ierr =  MPI_Comm_create(MPI_COMM_WORLD, group3, &comm3);
	if (ierr != MPI_SUCCESS)
	{
	    sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_create #2", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_FATAL, error_string);
	    fail++;
	}       /* Error Test  */

	ierr =  MPI_Group_rank(group3, &rank3);
	if (ierr != MPI_SUCCESS)
	{
	    sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_rank #2", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_FATAL, error_string);
	    fail++;
	}       /* Error Test  */



	if (MPITEST_me == 0)
	{
	    sprintf(info_buf, "Excluding rank %d in first comm, %d in second", i, j);
	    MPITEST_message(MPITEST_INFO1, info_buf);
	}

	/* ------------------------------------------------------------ */
	if (MPITEST_me == 0)
	{
	    sprintf(info_buf, "Allgather");
	    MPITEST_message(MPITEST_INFO2, info_buf);
	}

	if (rank2 != MPI_UNDEFINED)
	{
	    sleep(2);
	    value = 0;
	    for (k=0; k < MPITEST_nump - 1; k++)
		result_ary[k] = 1;
	    ierr =  MPI_Allgather(&value, 1, MPI_INT, result_ary, 1, MPI_INT, comm2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Allgather #1", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_fail = 0;
	    for (k=0; k < MPITEST_nump - 1; k++)
		if (result_ary[k] != 0)
		    loop_fail++;
	    if (loop_fail != 0)
	    {
		sprintf(info_buf, "MPI_Allgather #1 received %d bad values", loop_fail);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;
	}

	if (rank3 != MPI_UNDEFINED)
	{
	    value = -1;
	    for (k=0; k < MPITEST_nump - 1; k++)
		result_ary[k] = 1;
	    ierr =  MPI_Allgather(&value, 1, MPI_INT, result_ary, 1, MPI_INT, comm3);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Allgather #2", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_fail = 0;
	    for (k=0; k < MPITEST_nump - 1; k++)
		if (result_ary[k] != -1)
		    loop_fail++;
	    if (loop_fail != 0)
	    {
		sprintf(info_buf, "MPI_Allgather #2 received %d bad values", loop_fail);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;
	}

	/* ------------------------------------------------------------ */
	if (MPITEST_me == 0)
	{
	    sprintf(info_buf, "Allgatherv");
	    MPITEST_message(MPITEST_INFO2, info_buf);
	}

	if (rank2 != MPI_UNDEFINED)
	{
	    sleep(2);
	    value = 0;
	    for (k=0; k < MPITEST_nump - 1; k++)
	    {
		result_ary[k] = 0;
	    }
	    ierr =  MPI_Allgatherv(&value, 1, MPI_INT, result_ary, counts_ary, result_dsplsary, MPI_INT, comm2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Allgatherv #1", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_fail = 0;
	    for (k=0; k < MPITEST_nump - 1; k++)
		if (result_ary[k] != 0)
		    loop_fail++;
	    if (loop_fail != 0)
	    {
		sprintf(info_buf, "MPI_Allgatherv #1 received %d bad values", loop_fail);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;
	}

	if (rank3 != MPI_UNDEFINED)
	{
	    value = -1;
	    for (k=0; k < MPITEST_nump - 1; k++)
		result_ary[k] = 1;
	    ierr =  MPI_Allgatherv(&value, 1, MPI_INT, result_ary, counts_ary, result_dsplsary, MPI_INT, comm3);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Allgatherv #2", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_fail = 0;
	    for (k=0; k < MPITEST_nump - 1; k++)
		if (result_ary[k] != -1)
		    loop_fail++;
	    if (loop_fail != 0)
	    {
		sprintf(info_buf, "MPI_Allgatherv #2 received %d bad values", loop_fail);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;
	}

	/* ------------------------------------------------------------ */
	if (MPITEST_me == 0)
	{
	    sprintf(info_buf, "Allreduce");
	    MPITEST_message(MPITEST_INFO2, info_buf);
	}

	if (rank2 != MPI_UNDEFINED)
	{
	    value = 0;
	    result = 1;
	    sleep(2);
	    ierr =  MPI_Allreduce(&value, &result, 1, MPI_INT, MPI_SUM, comm2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Allreduce #1", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    if (result != 0)
	    {
		sprintf(info_buf, "MPI_Allreduce #1 received %d, expected 0", result);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;
	}

	if (rank3 != MPI_UNDEFINED)
	{
	    value = -1;
	    result = 1;
	    ierr =  MPI_Allreduce(&value, &result, 1, MPI_INT, MPI_SUM, comm3);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Allreduce #2", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    if (result != -1 *(MPITEST_nump-1))
	    {
		sprintf(info_buf, "MPI_Allreduce #2 received %d, expected %d", result, -1 *(MPITEST_nump-1));
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;
	}

	/* ------------------------------------------------------------ */
	if (MPITEST_me == 0)
	{
	    sprintf(info_buf, "Alltoall");
	    MPITEST_message(MPITEST_INFO2, info_buf);
	}

	if (rank2 != MPI_UNDEFINED)
	{
	    sleep(2);
	    for (k=0; k < MPITEST_nump - 1; k++)
	    {
		value_ary[k] = 0;
		result_ary[k] = 1;
	    }
	    ierr =  MPI_Alltoall(value_ary, 1, MPI_INT, result_ary, 1, MPI_INT, comm2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Alltoall #1", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_fail = 0;
	    for (k=0; k < MPITEST_nump - 1; k++)
		if (result_ary[k] != 0)
		    loop_fail++;
	    if (loop_fail != 0)
	    {
		sprintf(info_buf, "MPI_Alltoall #1 received %d bad values", loop_fail);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;
	}

	if (rank3 != MPI_UNDEFINED)
	{
	    for (k=0; k < MPITEST_nump - 1; k++)
	    {
		value_ary[k] = -1;
		result_ary[k] = 1;
	    }
	    ierr =  MPI_Alltoall(value_ary, 1, MPI_INT, result_ary, 1, MPI_INT, comm3);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Alltoall #2", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_fail = 0;
	    for (k=0; k < MPITEST_nump - 1; k++)
		if (result_ary[k] != -1)
		    loop_fail++;
	    if (loop_fail != 0)
	    {
		sprintf(info_buf, "MPI_Alltoall #2 received %d bad values", loop_fail);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;
	}

	/* ------------------------------------------------------------ */
	if (MPITEST_me == 0)
	{
	    sprintf(info_buf, "Alltoallv");
	    MPITEST_message(MPITEST_INFO2, info_buf);
	}

	if (rank2 != MPI_UNDEFINED)
	{
	    sleep(2);
	    for (k=0; k < MPITEST_nump - 1; k++)
	    {
		value_ary[k] = 0;
		result_ary[k] = 1;
	    }
	    ierr =  MPI_Alltoallv(value_ary, counts_ary, value_dsplsary, MPI_INT, result_ary, counts_ary, result_dsplsary, MPI_INT, comm2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Alltoallv #1", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_fail = 0;
	    for (k=0; k < MPITEST_nump - 1; k++)
		if (result_ary[k] != 0)
		    loop_fail++;
	    if (loop_fail != 0)
	    {
		sprintf(info_buf, "MPI_Alltoallv #1 received %d bad values", loop_fail);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;
	}

	if (rank3 != MPI_UNDEFINED)
	{
	    for (k=0; k < MPITEST_nump - 1; k++)
	    {
		value_ary[k] = -1;
		result_ary[k] = 1;
	    }
	    ierr =  MPI_Alltoallv(value_ary, counts_ary, value_dsplsary, MPI_INT, result_ary, counts_ary, result_dsplsary, MPI_INT, comm3);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Alltoallv #2", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_fail = 0;
	    for (k=0; k < MPITEST_nump - 1; k++)
		if (result_ary[k] != -1)
		    loop_fail++;
	    if (loop_fail != 0)
	    {
		sprintf(info_buf, "MPI_Alltoallv #2 received %d bad values", loop_fail);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;
	}

	/* ------------------------------------------------------------ */
	if (MPITEST_me == 0)
	{
	    sprintf(info_buf, "Bcast");
	    MPITEST_message(MPITEST_INFO2, info_buf);
	}

	if (rank2 != MPI_UNDEFINED)
	{
	    value = rank2;
	    sleep(2);
	    ierr =  MPI_Bcast(&value, 1, MPI_INT, 0, comm2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Bcast #1", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    if (value != 0)
	    {
		sprintf(info_buf, "MPI_Bcast #1 received %d, expected 0", value);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;
	}

	if (rank3 != MPI_UNDEFINED)
	{
	    value = -1 * rank3 - 1;
	    ierr =  MPI_Bcast(&value, 1, MPI_INT, 0, comm3);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Bcast #2", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    if (value != -1)
	    {
		sprintf(info_buf, "MPI_Bcast #2 received %d, expected %d", value, -1);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;
	}

	/* ------------------------------------------------------------ */
	if (MPITEST_me == 0)
	{
	    sprintf(info_buf, "Gather");
	    MPITEST_message(MPITEST_INFO2, info_buf);
	}

	if (rank2 != MPI_UNDEFINED)
	{
	    sleep(2);
	    value = 0;
	    for (k=0; k < MPITEST_nump - 1; k++)
		result_ary[k] = 1;
	    ierr =  MPI_Gather(&value, 1, MPI_INT, result_ary, 1, MPI_INT, 0, comm2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Gather #1", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_fail = 0;
	    if (rank2 == 0)
	    {
		for (k=0; k < MPITEST_nump - 1; k++)
		    if (result_ary[k] != 0)
			loop_fail++;
	    }
	    if (loop_fail != 0)
	    {
		sprintf(info_buf, "MPI_Gather #1 received %d bad values", loop_fail);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;
	}

	if (rank3 != MPI_UNDEFINED)
	{
	    value = -1;
	    for (k=0; k < MPITEST_nump - 1; k++)
		result_ary[k] = 1;
	    ierr =  MPI_Gather(&value, 1, MPI_INT, result_ary, 1, MPI_INT, 0, comm3);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Gather #2", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_fail = 0;
	    if (rank3 == 0)
	    {
		for (k=0; k < MPITEST_nump - 1; k++)
		    if (result_ary[k] != -1)
			loop_fail++;
	    }
	    if (loop_fail != 0)
	    {
		sprintf(info_buf, "MPI_Gather #2 received %d bad values", loop_fail);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;
	}

	/* ------------------------------------------------------------ */
	if (MPITEST_me == 0)
	{
	    sprintf(info_buf, "Gatherv");
	    MPITEST_message(MPITEST_INFO2, info_buf);
	}

	if (rank2 != MPI_UNDEFINED)
	{
	    sleep(2);
	    value = 0;
	    for (k=0; k < MPITEST_nump - 1; k++)
		result_ary[k] = 1;
	    ierr =  MPI_Gatherv(&value, 1, MPI_INT, result_ary, counts_ary, result_dsplsary, MPI_INT, 0, comm2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Gatherv #1", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_fail = 0;
	    if (rank2 == 0)
	    {
		for (k=0; k < MPITEST_nump - 1; k++)
		    if (result_ary[k] != 0)
			loop_fail++;
	    }
	    if (loop_fail != 0)
	    {
		sprintf(info_buf, "MPI_Gatherv #1 received %d bad values", loop_fail);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;
	}

	if (rank3 != MPI_UNDEFINED)
	{
	    value = -1;
	    for (k=0; k < MPITEST_nump - 1; k++)
		result_ary[k] = 1;
	    ierr =  MPI_Gatherv(&value, 1, MPI_INT, result_ary, counts_ary, result_dsplsary, MPI_INT, 0, comm3);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Gatherv #2", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_fail = 0;
	    if (rank3 == 0)
	    {
		for (k=0; k < MPITEST_nump - 1; k++)
		    if (result_ary[k] != -1)
			loop_fail++;
	    }
	    if (loop_fail != 0)
	    {
		sprintf(info_buf, "MPI_Gatherv #2 received %d bad values", loop_fail);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;
	}

	/* ------------------------------------------------------------ */
	if (MPITEST_me == 0)
	{
	    sprintf(info_buf, "Reduce");
	    MPITEST_message(MPITEST_INFO2, info_buf);
	}

	if (rank2 != MPI_UNDEFINED)
	{
	    sleep(2);
	    value = 0;
	    result = 1;
	    ierr =  MPI_Reduce(&value, &result, 1, MPI_INT, MPI_SUM, 0, comm2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Reduce #1", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    if ((rank2 == 0) && (result != 0))
	    {
		sprintf(info_buf, "MPI_Reduce #1 received %d, expected 0", result);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;
	}

	if (rank3 != MPI_UNDEFINED)
	{
	    value = -1;
	    result = 0;
	    ierr =  MPI_Reduce(&value, &result, 1, MPI_INT, MPI_SUM, 0, comm3);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Reduce #2", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    if ((rank3 == 0) && (result != -1 *(MPITEST_nump-1)))
	    {
		sprintf(info_buf, "MPI_Reduce #2 received %d, expected %d", result, -1 *(MPITEST_nump-1));
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;
	}

	/* ------------------------------------------------------------ */
	if (MPITEST_me == 0)
	{
	    sprintf(info_buf, "Reduce_scatter");
	    MPITEST_message(MPITEST_INFO2, info_buf);
	}

	if (rank2 != MPI_UNDEFINED)
	{
	    sleep(2);
	    for (k=0; k < MPITEST_nump - 1; k++)
		value_ary[k] = 0;
	    result = 7;
	    ierr =  MPI_Reduce_scatter(value_ary, &result, counts_ary, MPI_INT, MPI_SUM, comm2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Reduce_scatter #1", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    if (result != 0)
	    {
		sprintf(info_buf, "MPI_Reduce_scatter #1 received %d expected 0", result);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;
	}

	if (rank3 != MPI_UNDEFINED)
	{
	    for (k=0; k < MPITEST_nump - 1; k++)
		value_ary[k] = -1;
	    result = 7;
	    ierr =  MPI_Reduce_scatter(value_ary, &result, counts_ary, MPI_INT, MPI_SUM, comm3);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Reduce_scatter #2", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    if (result != -1*MPITEST_nump + 1)
	    {
		sprintf(info_buf, "MPI_Reduce_scatter #2 received %d expected %d", result, -1*MPITEST_nump + 1);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;
	}

	/* ------------------------------------------------------------ */
	if (MPITEST_me == 0)
	{
	    sprintf(info_buf, "Scan");
	    MPITEST_message(MPITEST_INFO2, info_buf);
	}

	if (rank2 != MPI_UNDEFINED)
	{
	    sleep(2);
	    value = 0;
	    result = 1;
	    ierr =  MPI_Scan(&value, &result, 1, MPI_INT, MPI_SUM, comm2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Scan #1", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    if (result != 0)
	    {
		sprintf(info_buf, "MPI_Scan #1 received %d, expected 0", result);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;
	}

	if (rank3 != MPI_UNDEFINED)
	{
	    value = -1;
	    result = 0;
	    ierr =  MPI_Scan(&value, &result, 1, MPI_INT, MPI_SUM, comm3);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Scan #2", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    expect = 0;
	    for (k=1; k <= rank3 + 1; k++)
		expect = expect - 1;
	    if (result != expect)
	    {
		sprintf(info_buf, "MPI_Scan #2 received %d, expected %d", result, expect);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;
	}

	/* ------------------------------------------------------------ */
	if (MPITEST_me == 0)
	{
	    sprintf(info_buf, "Scatter");
	    MPITEST_message(MPITEST_INFO2, info_buf);
	}

	if (rank2 != MPI_UNDEFINED)
	{
	    sleep(2);
	    if (rank2 == 0)
	    {
		for (k=0; k < MPITEST_nump - 1; k++)
		    value_ary[k] = 0;
	    }
	    result = 7;
	    ierr =  MPI_Scatter(value_ary, 1, MPI_INT, &result, 1, MPI_INT, 0, comm2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Scatter #1", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    if (result != 0)
	    {
		sprintf(info_buf, "MPI_Scatter #1 received %d expected 0", result);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;
	}

	if (rank3 != MPI_UNDEFINED)
	{
	    if (rank3 == 0)
	    {
		for (k=0; k < MPITEST_nump - 1; k++)
		    value_ary[k] = -1;
	    }
	    result = 7;
	    ierr =  MPI_Scatter(value_ary, 1, MPI_INT, &result, 1, MPI_INT, 0, comm3);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Scatter #2", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    if (result != -1)
	    {
		sprintf(info_buf, "MPI_Scatter #2 received %d expected -1", result);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;
	}

	/* ------------------------------------------------------------ */
	if (MPITEST_me == 0)
	{
	    sprintf(info_buf, "Scatterv");
	    MPITEST_message(MPITEST_INFO2, info_buf);
	}

	if (rank2 != MPI_UNDEFINED)
	{
	    sleep(2);
	    if (rank2 == 0)
	    {
		for (k=0; k < MPITEST_nump - 1; k++)
		    value_ary[k] = 0;
	    }
	    result = 7;
	    ierr =  MPI_Scatterv(value_ary, counts_ary, value_dsplsary, MPI_INT, &result, 1, MPI_INT, 0, comm2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Scatterv #1", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    if (result != 0)
	    {
		sprintf(info_buf, "MPI_Scatterv #1 received %d expected 0", result);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;
	}

	if (rank3 != MPI_UNDEFINED)
	{
	    if (rank3 == 0)
	    {
		for (k=0; k < MPITEST_nump - 1; k++)
		    value_ary[k] = -1;
	    }
	    result = 7;
	    ierr =  MPI_Scatterv(value_ary, counts_ary, value_dsplsary, MPI_INT, &result, 1, MPI_INT, 0, comm3);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Scatterv #2", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    if (result != -1)
	    {
		sprintf(info_buf, "MPI_Scatterv #2 received %d expected -1", result);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;
	}

	/* ------------------------------------------------------------ */

	/*
	 * All done - free the group & communicator under test.
	*/
	ierr =  MPI_Group_free(&group2);
	if (ierr != MPI_SUCCESS)
	{
	    sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_free #1", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_NONFATAL, error_string);
	    fail++;
	}       /* Error Test  */

	ierr = 0;
        if (comm2 != MPI_COMM_NULL)
	    ierr =  MPI_Comm_free(&comm2);
	if (ierr != MPI_SUCCESS)
	{
	    sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_free #1", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_NONFATAL, error_string);
	    fail++;
	}       /* Error Test  */

	ierr =  MPI_Group_free(&group3);
	if (ierr != MPI_SUCCESS)
	{
	    sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_free #2", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_NONFATAL, error_string);
	    fail++;
	}       /* Error Test  */

	ierr = 0;
        if (comm3 != MPI_COMM_NULL)
	    ierr =  MPI_Comm_free(&comm3);
	if (ierr != MPI_SUCCESS)
	{
	    sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_free #2", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_NONFATAL, error_string);
	    fail++;
	}       /* Error Test  */


    } /* Loop through ranks excluded */


    /*
     * Free the group of MPI_COMM_WORLD.
    */
    ierr =  MPI_Group_free(&group);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_free #3", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */

    /*
     * Free buffers
    */
    free(result_ary);
    free(result_dsplsary);
    free(value_ary);
    free(value_dsplsary);
    free(counts_ary);

    /* report overall results  */

    MPITEST_report(loop_cnt - fail, fail, 0, testname);

    MPI_Finalize();

    return fail;

}/* main() */

