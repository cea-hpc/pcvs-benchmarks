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
tested here.  In addition, MPI_Comm_dup is tested with intercommunicators here,
also verifying that cached information in the parent communicator is
inhereted by the child communicator.

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
	 key,		/* MPI key                         */
	 value,         /* Value for the key               */
	*attr,		/* Returned value of the attribute */
	 found,		/* From MPI_Attr_get               */
	 color,         /* For MPI_Comm_split              */
         lrank,         /* Rank in the current comm        */
	 result,
	 resultinter,
	 size,
	 send1,		/* Used for traffic while          */
	 send2,         /*  generating the intercomm       */
	 send3,
	 recv1,
	 recv2,
	 recv3;

    MPI_Comm comm,      /* Communicator under test         */
	     commsplit, /* For split communicator          */
	     comminter, /* For created intercommunicator   */
	     comm2;     /* Dup of intercommunicator        */

    MPI_Status stat1,   /* Used for traffic while          */
	       stat2,   /*  generating the intercomm       */
	       stat3;

    MPI_Request req1,   /* Used for traffic while          */
		req2,   /*  generating the intercomm       */
		req3;

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


    sprintf(testname, "MPI_Intercomm_create1");

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
	    if ((result == 1) || (test_nump < 2))
	    {
	    	MPITEST_free_communicator(comm_type, &comm);
		continue;
	    }

	    /* Split the communicator in half */
	    color = MPITEST_current_rank%2;
	    /* Get the rank of this process in comm.  The 
               call to MPI_Intercomm_create depends on the split
               communicator being ordered in the same way as comm;
               it is possible that comm is ordered differently from
               MPI_COMM_WORLD (the old code used MPITEST_me, which 
               is rank in MPI_COMM_WORLD, instead of lrank) */
            MPI_Comm_rank( comm, &lrank );
	    ierr = MPI_Comm_split(comm, color, lrank, &commsplit);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_split (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;



	    /* Put some messages on the communicator before creating
	       the intercomm to ensure the correct TAG is used */
	    if (MPITEST_current_rank < 2)
	    {
		send1 = 19;
		send2 = 29;
		send3 = 29;
		ierr = MPI_Isend(&send1, 1, MPI_INT, 1 - MPITEST_current_rank,
			  26, comm, &req1);
		ierr = MPI_Isend(&send2, 1, MPI_INT, 1 - MPITEST_current_rank,
			  28, comm, &req2);
		ierr = MPI_Isend(&send3, 1, MPI_INT, 1 - MPITEST_current_rank,
			   0, comm, &req3);
	    }

	    /* Now combine the two new communicators to create an
	       intercommunicator */
	    ierr = MPI_Intercomm_create(commsplit, 0, comm, 1-color, 27,
			&comminter);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Intercomm_create (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;


	    /* Now receive the messages above and ensure they are intact */
	    if (MPITEST_current_rank < 2)
	    {
		ierr = MPI_Recv(&recv1, 1, MPI_INT, 1 - MPITEST_current_rank,
			  26, comm, &stat1);
		ierr = MPI_Recv(&recv2, 1, MPI_INT, 1 - MPITEST_current_rank,
			  28, comm, &stat2);
		ierr = MPI_Recv(&recv3, 1, MPI_INT, 1 - MPITEST_current_rank,
			   0, comm, &stat3);
		ierr = MPI_Wait(&req1, &stat1);
		ierr = MPI_Wait(&req2, &stat2);
		ierr = MPI_Wait(&req3, &stat3);
		if ((recv1 != send1) || (recv2 != send2) || 
		    (recv3 != send3))
		{
		    sprintf(info_buf, "Receive data corrupted by MPI_Intercomm_create: %d/%d/%d (expected 19/29/39) (commm_index %d)", 
			    recv1, recv2, recv3, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    fail++;
		}
	    }
	

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



	    /* Now set a value for the attribute */
	    value = 27;
	    ierr = MPI_Attr_put(comminter, key, &value);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_put", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;



	    /* Now get the value for the attribute; it should be there */
	    ierr = MPI_Attr_get(comminter, key, &attr, &found);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_get(comminter)", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    else if (found != 1)
	    {
		fail++;
		sprintf(info_buf, "MPI_Attr_get(comminter) did not find attribute (comm_index %d).", comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	    }
	    else if (*attr != 27)
	    {
		fail++;
		sprintf(info_buf, "MPI_Attr_get(comminter) returned value %d, expected 27 (comm_index %d).", *attr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
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

	    /* Size should be half the original, plus one if there are an
	       odd number of ranks and this is an even-numbered rank */
	    result = test_nump / 2;
	    if ((test_nump % 2 == 1) && (MPITEST_current_rank % 2 == 0))
		result++;
	    if (resultinter != result)
	    {
		sprintf(info_buf, "MPI_Comm_size returned %d, expected %d (comm_index %d)", resultinter, result, comm_index);
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

	    result = test_nump - result;
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
	    if (resultinter != MPITEST_current_rank/2)
	    {
		sprintf(info_buf, "MPI_Comm_rank returned %d, expected %d (comm_index %d)",
			resultinter, MPITEST_current_rank/2, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;

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


	   /* Now get the value for the attribute; it should be there */
	    attr = 0;
	    ierr = MPI_Attr_get(comm2, key, &attr, &found);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_get(comm2)", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    else if (found != 1)
	    {
		fail++;
		sprintf(info_buf, "MPI_Attr_get(comm2) did not find attribute (comm_index %d).", comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	    }
	    else if (*attr != 27)
	    {
		fail++;
		sprintf(info_buf, "MPI_Attr_get(comm2) returned value %d, expected 27 (comm_index %d).", *attr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	    }
	    loop_cnt++;



	    /* Now delete the attribute */
	    ierr = MPI_Attr_delete(comm2, key);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_delete after delete", ierr);
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
