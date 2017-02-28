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
			  Test for MPI_Comm_dup

This test verifies that MPI_Comm_dup creates a seperate commuicator, and
that message traffic is sorted by communicator.  Each configured
communicator is duplicated and tested.

As a consequence of this test, MPI_Comm_free, MPI_Comm_compare, MPI_Comm_size
MPI_Comm_test_inter and MPI_Comm_rank are also tested here.  Testing of cached
information is done in the tests for cached information calls.

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
	 intercomm,     /* result of MPI_Comm_test_inter   */
	 result,
	 resultdup,
	 buffer1,
	 buffer2,
	 buffer3,
	 size;

    MPI_Request request1,
	        request2,
	        request3;

    MPI_Status  status1,
		status2,
		status3;

    MPI_Comm comm,      /* Communicator under test         */
	     comm2;     /* For dup'ing communicator        */

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


    sprintf(testname, "MPI_Comm_dup");

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

	    /* Dup the communicator */
	    ierr = MPI_Comm_dup(comm, &comm2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_dup (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;


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
	    else if (result != MPI_CONGRUENT)
	    {
		sprintf(info_buf, "MPI_Comm_compare returned %d, expected MPI_CONGRUENT (comm_index %d)", result, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;


	    /* Check for intercommunicator */
	    ierr = MPI_Comm_test_inter(comm, &intercomm);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_test_inter #1 (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;

	    ierr = MPI_Comm_test_inter(comm2, &resultdup);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_test_inter #2 (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    else if (intercomm != resultdup)
	    {
		sprintf(info_buf, "MPI_Comm_test_inter returned %d, expected %d (comm_index %d)", resultdup, result, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;

	    /* Check the size */
	    ierr = MPI_Comm_size(comm2, &resultdup);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_size #1 (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;

	    if (intercomm == 1) /* an intercommunicator -
			           test_nump will be too big */
	    {
		ierr = MPI_Comm_size(comm, &result);
		if (ierr != MPI_SUCCESS)
		{
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_size #2 (comm_index %d)", ierr, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_NONFATAL, error_string);
		    fail++;
		}       /* Error Test  */
	    }
	    else result = test_nump;
	    if (result != resultdup)
	    {
		sprintf(info_buf, "MPI_Comm_size returned %d, expected %d (comm_index %d)", resultdup, result, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;


	    /* Check my rank */
	    ierr = MPI_Comm_rank(comm2, &resultdup);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_rank (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    else if (resultdup != MPITEST_current_rank)
	    {
		sprintf(info_buf, "MPI_Comm_rank returned %d, expected %d (comm_index %d)", resultdup, MPITEST_current_rank, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;


	    /* Now Isend 3 messages from rank 0 to rank 1, one per communicator.
	       Recv them in the opposite order, to make sure messages are
	       filtered to the proper communicator. To keep things simple
	       here, we'll test intercommunicators in a different test.
	       Repeat for Send */
	    if ((test_nump >= 2) && (intercomm == 0))
	    {
		if (MPITEST_current_rank == 0) /* Sender */
		{
		    buffer1=1;
		    buffer2=2;
		    buffer3=3;
		    MPI_Isend(&buffer1, 1, MPI_INT, 1, 1, comm,  &request1);
		    MPI_Isend(&buffer2, 1, MPI_INT, 1, 1, comm2, &request2);
		    MPI_Isend(&buffer3, 1, MPI_INT, 1, 1, comm2, &request3);
		    MPI_Barrier(comm2);
		    MPI_Wait(&request1, &status1);
		    MPI_Wait(&request2, &status2);
		    MPI_Wait(&request3, &status3);

		    MPI_Send(&buffer1, 1, MPI_INT, 1, 1, comm);
		    MPI_Send(&buffer2, 1, MPI_INT, 1, 1, comm);
		    MPI_Send(&buffer3, 1, MPI_INT, 1, 1, comm2);
		}
		else if (MPITEST_current_rank == 1) /* Receiver */
		{
		    buffer1=0;
		    buffer2=0;
		    buffer3=0;
		    MPI_Barrier(comm2);
		    MPI_Irecv(&buffer2, 1, MPI_INT, 0, 1, comm2, &request2);
		    MPI_Irecv(&buffer3, 1, MPI_INT, 0, 1, comm2, &request3);
		    MPI_Irecv(&buffer1, 1, MPI_INT, 0, 1, comm,  &request1);
		    MPI_Wait(&request1, &status1);
		    MPI_Wait(&request2, &status2);
		    MPI_Wait(&request3, &status3);
		    if ((buffer1 != 1) || (buffer2 != 2) || (buffer3 != 3))
		    {
			sprintf(info_buf, "Receive comms misfiltered messages (Isend): %d/%d/%d, expected 1/2/3 (comm_index %d)",
				buffer1, buffer2, buffer3, comm_index);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
			fail++;
		    }
		    loop_cnt++;

		    buffer1=0;
		    buffer2=0;
		    buffer3=0;
		    MPI_Irecv(&buffer1, 1, MPI_INT, 0, 1, comm,  &request2);
		    MPI_Irecv(&buffer3, 1, MPI_INT, 0, 1, comm2, &request3);
		    MPI_Irecv(&buffer2, 1, MPI_INT, 0, 1, comm,  &request1);
		    MPI_Wait(&request1, &status1);
		    MPI_Wait(&request2, &status2);
		    MPI_Wait(&request3, &status3);
		    if ((buffer1 != 1) || (buffer2 != 2) || (buffer3 != 3))
			{
			    sprintf(info_buf, "Receive comms misfiltered messages (Send): %d/%d/%d, expected 1/2/3 (comm_index %d)",
				    buffer1, buffer2, buffer3, comm_index);
			    MPITEST_message(MPITEST_NONFATAL, info_buf);
			    fail++;
			}
		    loop_cnt++;
		}
		else
		    MPI_Barrier(comm2);
	    }

	    /* Lastly, leave a communication outstanding, free the comm,
 	       then complete the communication */
	    if ((test_nump >= 2) && (intercomm == 0))
	    {
		if (MPITEST_current_rank == 0) /* Sender */
		{
		    MPI_Isend(&buffer1, 1, MPI_INT, 1, 1, comm2, &request1);
		}
		else if (MPITEST_current_rank == 1) /* Receiver */
		{
		    buffer1=0;
		    MPI_Irecv(&buffer1, 1, MPI_INT, 0, 1, comm2, &request1);
		}
	    }

	    /* Now free the dup'ed communicator */
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


	    /* Now finish the communication started above */
	    if ((test_nump >= 2) && (intercomm == 0))
	    {
		if (MPITEST_current_rank == 0) /* Sender */
		{
		    MPI_Wait(&request1, &status1);
		}
		else if (MPITEST_current_rank == 1) /* Receiver */
		{
		    MPI_Wait(&request1, &status1);
		    if (buffer1 != 1)
		    {
			sprintf(info_buf, "Receive while freeing comm received incorrect data %d, expected 1 (comm_index %d)",
				buffer1, comm_index);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
			fail++;
		    }
	    	    loop_cnt++;
		}
	    }
	    MPITEST_free_communicator(comm_type, &comm);

	} /* Node is in this communicator */

    } /* Communicator loop */


    /* report overall results  */

    MPITEST_report(loop_cnt - fail, fail, verify, testname);

    MPI_Finalize();

    return fail;

}/* main() */
