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
			  Test for MPI_Intercomm_create

This test verifies that MPI_Intercomm_create combines two eperate commuicators,
and that message traffic is sorted by communicator.  Each configured intra-
communicator (with more than 2 ranks) is split, an intercommunicator is
created and tested.

There are 2 create tests.  The difference is communicators are split the
following way:
 1)  split in half
 2)  split in thirds: rank 0 in 1 comm, ranks 2 through (size - 1) in second
	comm, rank 1 not in either.

As a consequence of this test, MPI_Comm_free, MPI_Comm_compare, MPI_Comm_size,
MPI_Comm_remote_size, MPI_Comm_rank, MPI_Comm_test_inter and rank are also
tested here.  In addition, MPI_Comm_dup is tested with intercommunicators here.

Test history:
   1  08/28/96     gt       Original version

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
	 test_nump,     /* number of ranks in current comm */
	 comm_index,    /* array index of current comm     */
	 comm_type,     /* index of current comm type      */
	 comm_count,    /* number of communicators to test */
	 type_count,    /* loop counter for data type loop */
	 color,         /* For MPI_Comm_split              */
	 result,
	 resultsplit,
	 resultinter,
	 buffer1,
	 buffer2,
	 buffer3,
	 size;

    MPI_Comm comm,      /* Communicator under test         */
	     commsplit, /* For split communicator          */
	     comminter, /* For created intercommunicator   */
	     comm2;     /* Dup of intercommunicator        */

    char
	  info_buf[256],/* buffer for passing mesages to MPITEST  */
	  testname[64]; /* the name of this test           */
    char error_string[MPI_MAX_ERROR_STRING];

    /*-----------------------------  MPI_Init  ------------------------------*/
    ierr = MPI_Init(&argc, &argv);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr);
	MPITEST_message(MPITEST_FATAL, info_buf);
    }


    sprintf(testname, "MPI_Intercomm_create2");

    /*-----------------------------  MPITEST_init  --------------------------*/
    MPITEST_init(argc, argv);
    if (MPITEST_me == 0)
    {
	sprintf(info_buf, "Starting %s test", testname);
	MPITEST_message(MPITEST_INFO0, info_buf);
    }

    /* set the global error counter */
    fail = 0;
    verify = 0;
    loop_cnt = 0;

    /* Loop through the configured communicators */
    for (comm_count=0; comm_count<MPITEST_num_comm_sizes(); comm_count++)
    {
		comm_index = MPITEST_get_comm_index(comm_count);
		comm_type = MPITEST_get_comm_type(comm_count);

		test_nump = MPITEST_get_communicator(comm_type, comm_index, &comm);
	
	/* Only test if this node is part of the current communicator */
	if (MPITEST_current_rank != MPI_UNDEFINED)
	{
		fprintf(stdout,"debut count = %d\n", comm_count);fflush(stdout);
	    /* Check for intercommunicator and skip (continue) if so;
	       intercomms cannot be split  */
	    ierr = MPI_Comm_test_inter(comm, &result);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_test_inter (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    if ((result == 1) || (test_nump < 3))
	    {
	    	MPITEST_free_communicator(comm_type, &comm);
		continue;
	    }



	    /* Split the communicator as above */
	    if (MPITEST_current_rank == 0)
		color = test_nump-1;
	    else if (MPITEST_current_rank == 1)
		color = MPI_UNDEFINED;
	    else color = 0;
	    ierr = MPI_Comm_split(comm, color, MPITEST_me, &commsplit);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_split (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;


	    if (MPITEST_current_rank == 1)
	    {
		if (commsplit != MPI_COMM_NULL)
		{
		    fail++;
		    sprintf(info_buf, "MPI_Comm_split w/ color = MPI_UNDEFINED returned %d, expected MPI_COMM_NULL (comm_index %d)", comm, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		}
	    }
	    else
	    {
		/* Now combine the two new communicators to create an
		   intercommunicator */
		if (MPITEST_current_rank == 0)
		    /* First split, 1 rank
		       0 must be local leader, last rank is remote leader */
		    ierr = MPI_Intercomm_create(commsplit, 0, comm,
						test_nump-1, 65534, &comminter);
		else
		     /* Second split, all ranks but 2
			last rank is local leader, 0 is remote leader */
		    ierr = MPI_Intercomm_create(commsplit, test_nump-3, comm,
						0, 65534, &comminter);
		if (ierr != MPI_SUCCESS)
		{
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_Intercomm_create (comm_index %d)", ierr, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_FATAL, error_string);
		    fail++;
		}       /* Error Test  */
		loop_cnt++;



		/* Compare the inter communicator with the original */
		ierr = MPI_Comm_compare(comm, comminter, &result);
		if (ierr != MPI_SUCCESS)
		{
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_compare (comm_index %d)", ierr, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_FATAL, error_string);
		    fail++;
		}       /* Error Test  */
		loop_cnt++;

		if (result != MPI_UNEQUAL)
		{
		    sprintf(info_buf, "MPI_Comm_compare returned %d, expected MPI_UNEQUAL (comm_index %d)", result, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    fail++;
		}
		loop_cnt++;



		/* Test for intercomm - we know it is */
		ierr = MPI_Comm_test_inter(comminter, &resultinter);
		if (ierr != MPI_SUCCESS)
		{
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_test_inter #2 (comm_index %d)", ierr, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_FATAL, error_string);
		    fail++;
		}       /* Error Test  */
		loop_cnt++;
		if (resultinter != 1)
		{
		    sprintf(info_buf, "MPI_Comm_test_inter returned %d, expected 1 (comm_index %d)", resultinter, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    fail++;
		}
		loop_cnt++;



		/* Check the size */
		fprintf(stderr,"appel MPI_Comm_size comm %d,  me = %d, current = %d, comm_count = %d\n", comminter, MPITEST_me, MPITEST_current_rank, comm_count);
		ierr = MPI_Comm_size(comminter, &resultinter);
		if (ierr != MPI_SUCCESS)
		{
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_size (comm_index %d)", ierr, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_FATAL, error_string);
		    fail++;
		}       /* Error Test  */
		loop_cnt++;

		/* Size should 1 (rank 0) or test_nump-2 (all others) */
		if (MPITEST_current_rank == 0)
		    result = 1;
		else
		    result = test_nump - 2;
		if (resultinter != result)
		{
		    sprintf(info_buf, "MPI_Comm_size returned %d, expected %d (comm_index %d, comm %d)", resultinter, result, comm_index, comminter);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    fail++;
		}
		loop_cnt++;



		/* Check the remote size */
		ierr = MPI_Comm_remote_size(comminter, &resultinter);
		if (ierr != MPI_SUCCESS)
		{
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_remote_size (comm_index %d)", ierr, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_FATAL, error_string);
		    fail++;
		}       /* Error Test  */
		loop_cnt++;

		if (MPITEST_current_rank == 0)
		    result = test_nump - 2;
		else
		    result = 1;
		if (resultinter != result)
		{
		    sprintf(info_buf, "MPI_Comm_remote_size returned %d, expected %d (comm_index %d)", resultinter, result, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    fail++;
		}
		loop_cnt++;



		/* Check the rank - it should be the same value as in the split
		   communicator */
		ierr = MPI_Comm_rank(comminter, &resultinter);
		if (ierr != MPI_SUCCESS)
		{
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_rank (comm_index %d)", ierr, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_FATAL, error_string);
		    fail++;
		}       /* Error Test  */
		loop_cnt++;
		result = 0;
		if (MPITEST_current_rank > 0)
		    result = MPITEST_current_rank-2;
		if (resultinter != result)
		{
		    sprintf(info_buf, "MPI_Comm_rank returned %d, expected %d (comm_index %d)",
			    resultinter, result, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    fail++;
		}
		loop_cnt++;



#if 0
	    /* Try the new communicator */
	    if (resultsplit == 0)
		result = color;
	    ierr = MPI_Bcast(&result, 1, MPI_INT, 0, commsplit);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Bcast", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    else if (result != color)
	    {
		fail++;
		sprintf(info_buf, "MPI_Bcast received incorrect data %d, expected %d (comm_index %d).", result, color, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	    }
	    loop_cnt++;
#endif



		/* Try to dup the new intercommunicator */
		ierr = MPI_Comm_dup(comminter, &comm2);
		if (ierr != MPI_SUCCESS)
		{
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_dup (comm_index %d)", ierr, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_NONFATAL, error_string);
		    fail++;
		}       /* Error Test  */
		loop_cnt++;



		/* Compare the two inter-communicators */
		ierr = MPI_Comm_compare(comminter, comm2, &result);
		if (ierr != MPI_SUCCESS)
		{
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_compare #2 (comm_index %d)", ierr, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_FATAL, error_string);
		    fail++;
		}       /* Error Test  */
		loop_cnt++;

		if (result != MPI_CONGRUENT)
		{
		    sprintf(info_buf, "MPI_Comm_compare #2 returned %d, expected MPI_CONGRUENT (comm_index %d)", result, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    fail++;
		}
		loop_cnt++;



		/* Now free all the communicators */
		ierr = MPI_Comm_free(&commsplit);
		if (ierr != MPI_SUCCESS)
		{
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_free(commsplit) (comm_index %d)", ierr, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_NONFATAL, error_string);
		    fail++;
		}       /* Error Test  */
		loop_cnt++;

		if (commsplit != MPI_COMM_NULL)
		{
		    sprintf(info_buf, "comm = %d after MPI_Comm_free(commsplit), expected MPI_COMM_NULL (comm_index %d)", commsplit, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    fail++;
		}
		loop_cnt++;

		ierr = MPI_Comm_free(&comminter);
		if (ierr != MPI_SUCCESS)
		{
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_free(comminter) (comm_index %d)", ierr, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_NONFATAL, error_string);
		    fail++;
		}       /* Error Test  */
		loop_cnt++;

		if (comminter != MPI_COMM_NULL)
		{
		    sprintf(info_buf, "comm = %d after MPI_Comm_free(comminter), expected MPI_COMM_NULL (comm_index %d)", comminter, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    fail++;
		}
		loop_cnt++;


		ierr = MPI_Comm_free(&comm2);
		if (ierr != MPI_SUCCESS)
		{
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_free(comm2) (comm_index %d)", ierr, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_NONFATAL, error_string);
		    fail++;
		}       /* Error Test  */
		loop_cnt++;

		if (comm2 != MPI_COMM_NULL)
		{
		    sprintf(info_buf, "comm = %d after MPI_Comm_free(comm2), expected MPI_COMM_NULL (comm_index %d)", comm2, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    fail++;
		}
		loop_cnt++;

	    } /* Rank 1 not in this test */

	    MPITEST_free_communicator(comm_type, &comm);

	} /* Node is in this communicator */
    } /* Communicator loop */

    /* report overall results  */

    MPITEST_report(loop_cnt - fail, fail, verify, testname);

    MPI_Finalize();

    return fail;

}/* main() */
