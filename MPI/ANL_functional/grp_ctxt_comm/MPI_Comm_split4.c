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
			  Test for MPI_Comm_split

This test verifies that MPI_Comm_split creates seperate commuicators, and
that message traffic is sorted by communicator.  Each configured intra-
communicator is split and tested.

There are 4 split tests.  The difference is communicators are split the
following way:
 1)  each rank into a new communicator
 2)  split into one (same color for all)
 3)  split in half
 4)  split in thirds, 2 new comms and 1/3 not in any comm.

As a consequence of this test, MPI_Comm_free, MPI_Comm_compare, MPI_Comm_size
MPI_Comm_test_inter and MPI_Comm_rank are also tested here.  In addition,
this test verifies that cached information in the parent communicator is
not inhereted by the split communicators.

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
	 key,		/* MPI key                         */
	 found,		/* From MPI_Attr_get               */
	*attr,
	 color,		/* For use by Split_comm           */
	 result,
	 resultsplit,
	 buffer1,
	 buffer2,
	 buffer3,
	 size;

    MPI_Comm comm,      /* Communicator under test         */
	     comm2;     /* For split communicator          */

    MPI_Aint extra;     /* For keyval creation/use         */

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


    sprintf(testname, "MPI_Comm_split4");

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


    /* Create a keyval */
    ierr = MPI_Keyval_create(MPI_DUP_FN, MPI_NULL_DELETE_FN, &key, &extra);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Keyval_create", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */



    /* Loop through the configured communicators */
    for (comm_count=0; comm_count<MPITEST_num_comm_sizes(); comm_count++)
    {
	comm_index = MPITEST_get_comm_index(comm_count);
	comm_type = MPITEST_get_comm_type(comm_count);

	test_nump = MPITEST_get_communicator(comm_type, comm_index, &comm);

	/* Only test if this node is part of the current communicator */
	if (MPITEST_current_rank != MPI_UNDEFINED)
	{
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


	    /* Now set a value for the attribute */
	    result = 0;
	    ierr = MPI_Attr_put(comm, key, &result);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_put (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */


	    /* Split the communicator, First third into a comm,
	       second third into a second comm, and remainder NOT
	       in a communicator (using MPI_UNDEFINED) */
	    result = test_nump/3;
	    if (MPITEST_current_rank < result)
		color = 1;
	    else if (MPITEST_current_rank < 2*result)
		color = 0;
	    else color = MPI_UNDEFINED;

	    ierr = MPI_Comm_split(comm, color, 0, &comm2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_split (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;

	    if (color == MPI_UNDEFINED)
	    {
		if (comm2 != MPI_COMM_NULL)
		{
		    sprintf(info_buf, "MPI_Comm_split(MPI_UNDEFINED) did not return MPI_COMM_NULL (comm_index %d)", comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    fail++;
		}
		loop_cnt++;
	    }
	    else /* In a new communicator */
	    {
		/* Compare the two communicators */
		ierr = MPI_Comm_compare(comm, comm2, &result);
		if (ierr != MPI_SUCCESS)
		{
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_compare (comm_index %d)", ierr, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_NONFATAL, error_string);
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


		/* Test for intercomm - we know it isn't */
		ierr = MPI_Comm_test_inter(comm2, &resultsplit);
		if (ierr != MPI_SUCCESS)
		{
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_test_inter #2 (comm_index %d)", ierr, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_NONFATAL, error_string);
		    fail++;
		}       /* Error Test  */
		else if (resultsplit != 0)
		{
		    sprintf(info_buf, "MPI_Comm_test_inter returned %d, expected 0 (comm_index %d)", resultsplit, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    fail++;
		}
		loop_cnt++;


		/* Check the size */
		ierr = MPI_Comm_size(comm2, &resultsplit);
		if (ierr != MPI_SUCCESS)
		{
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_size (comm_index %d)", ierr, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_NONFATAL, error_string);
		    fail++;
		}       /* Error Test  */
		loop_cnt++;

		/* Size should be one-third the original */
		result = test_nump / 3;
		if (resultsplit != result)
		{
		    sprintf(info_buf, "MPI_Comm_size returned %d, expected %d (comm_index %d)", resultsplit, result, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    fail++;
		}
		loop_cnt++;


		/* Check the rank */
		ierr = MPI_Comm_rank(comm2, &resultsplit);
		if (ierr != MPI_SUCCESS)
		{
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_rank (comm_index %d)", ierr, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_NONFATAL, error_string);
		    fail++;
		}       /* Error Test  */
		loop_cnt++;

		result = MPITEST_current_rank;
		if (result >= test_nump / 3)
		    result = result - test_nump / 3;
		if (resultsplit != result)
		{
		    sprintf(info_buf, "MPI_Comm_rank returned %d, expected %d (comm_index %d)",
			resultsplit, result, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    fail++;
		}
		loop_cnt++;


		/* Try the new communicator */
		if (resultsplit == 0)
		    result = color;
		ierr = MPI_Bcast(&result, 1, MPI_INT, 0, comm2);
		if (ierr != MPI_SUCCESS)
		{
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_Bcast (comm_index %d)", ierr, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_NONFATAL, error_string);
		    fail++;
		}       /* Error Test  */
		else if (result != color)
		{
		    fail++;
		    sprintf(info_buf, "MPI_Bcast received incorrect data %d, expected %d (comm_index %d)", result, color, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		}
		loop_cnt++;


		/* Now get the value for the attribute; it should NOT be there */
		ierr = MPI_Attr_get(comm2, key, &attr, &found);
		if (ierr != MPI_SUCCESS)
		{
		   sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_get (comm_index %d)", ierr, comm_index);
		   MPITEST_message(MPITEST_NONFATAL, info_buf);
		   MPI_Error_string(ierr, error_string, &size);
		   MPITEST_message(MPITEST_NONFATAL, error_string);
		   fail++;
		}       /* Error Test  */
		else if (found != 0)
		{
		   fail++;
		   sprintf(info_buf, "MPI_Attr_get found attribute in split comm; it should not (comm_index %d)", comm_index);
		   MPITEST_message(MPITEST_NONFATAL, info_buf);
		}
		loop_cnt++;


		/* Now free the split the communicator */
		ierr = MPI_Comm_free(&comm2);
		if (ierr != MPI_SUCCESS)
		{
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_free (comm_index %d)", ierr, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_NONFATAL, error_string);
		    fail++;
		}       /* Error Test  */
		else if (comm2 != MPI_COMM_NULL)
		{
		    sprintf(info_buf, "comm = %d after MPI_Comm_free, expected MPI_COMM_NULL (comm_index %d)", comm2, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    fail++;
		}
		loop_cnt++;

	    } /* in new (split) commuicator */

	    /* Now delete the attribute */
	    ierr = MPI_Attr_delete(comm, key);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_delete after delete (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;


	    MPITEST_free_communicator(comm_type, &comm);

	} /* Node is in this communicator */

    } /* Communicator loop */

    /* Free the Keyval */
    ierr = MPI_Keyval_free(&key);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Keyval_free", ierr
);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */

    /* report overall results  */

    MPITEST_report(loop_cnt - fail, fail, verify, testname);

    MPI_Finalize();

    return fail;

}/* main() */
