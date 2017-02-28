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
		  Test for MPI_Recv() from MPI_PROC_NULL

This tests the basic blocking MPI_Recv operation from MPI_PROC_NULL.  No
data should be received, and the received source should be MPI_PROC_NULL
and the tag MPI_TAG_ANY.

This test may be run in any communicator, with any data type, and with
any non-negative message length.

The MPITEST environment provides looping over communicator size and
type, message length, and data type.  The properties of the loops are
encoded in configuration arrays in the file mpitest_cfg.h .  See the
MPITEST README for further details.
******************************************************************************/

#include "mpitest_cfg.h"
#include "mpitest.h"

int main(int argc, char *argv[])
{

    int
        byte_length,	    /* The length of the current buffer in bytes     */
	cnt_len,            /* received length returned by MPI_Get_Count     */
        comm_count,	    /* loop counter for communicator loop            */
        comm_index,	    /* the array index of the current comm           */
        comm_type,	    /* the index of the current communicator type    */
        error,    	    /* errors from one MPI call                      */
        fail,   	    /* counts total number of failures               */
        loop_fail,   	    /* counts number of failures in loop             */
        i, j,	            /* utility loop index variables                  */
        ierr,	            /* return value from MPI calls                   */
        length,	            /* The length of the current buffer              */
        length_count,	    /* loop counter for message length loop          */
        loop_cnt,	    /* counts total number of loops through test     */
        max_byte_length,    /* maximum buffer length in bytes                */
        max_length,	    /* max buffer length specified in config. file   */
        size,	            /* return size from MPI_Error_string             */
        test_nump,	    /* The number of nodes in current communicator   */
        test_type,	    /* the index of the current buffer type          */
        type_count;	    /* loop counter for data type loop               */


        void *recv_buffer;  /* message buffer                                */

        char
	  info_buf[256],    /* buffer for passing mesages to MPITEST         */
	  testname[64];     /* the name of this test                         */
    char error_string[MPI_MAX_ERROR_STRING];

        MPI_Comm
	  comm;	            /* MPI communicator                              */

	MPI_Status
	  recv_stat;        /* MPI status structure			     */
    /*-----------------------------  MPI_Init  ------------------------------*/
    ierr = MPI_Init(&argc, &argv);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr);
	MPITEST_message(MPITEST_FATAL, info_buf);
    }


    sprintf(testname, "MPI_Recv_null: all Recv from MPI_PROC_NULL");

    /*-----------------------------  MPITEST_init  --------------------------*/
    MPITEST_init(argc, argv);
    if (MPITEST_me == 0)
    {
	sprintf(info_buf, "Starting %s test", testname);
	MPITEST_message(MPITEST_INFO0, info_buf);
    }

    /* set the global error counter */
    fail = 0;
    loop_cnt = 0;

    max_byte_length = MPITEST_get_max_message_length();

    /*--------------------------  Loop over Communicators  ------------------*/

    for (comm_count = 0; comm_count < MPITEST_num_comm_sizes(); comm_count++)
    {
	comm_index = MPITEST_get_comm_index(comm_count);
	comm_type = MPITEST_get_comm_type(comm_count);

	test_nump = MPITEST_get_communicator(comm_type, comm_index, &comm);

	/* Skip everything if not a member of this communicator */
	if (MPITEST_current_rank != MPI_UNDEFINED)
	{

	    /*------------------  Loop over Data Types  ---------------------*/

	    for (type_count = 0; type_count < MPITEST_num_datatypes(); type_count++)
	    {
		test_type = MPITEST_get_datatype(type_count);

		/* convert the number of bytes in the maximum length message */
		/* into the number of elements of the current type */

		max_length = MPITEST_byte_to_element(test_type, max_byte_length);

		/* Allocate send buffer */
		MPITEST_get_buffer(test_type, max_length, &recv_buffer);

		/*-------------  Loop over Message Lengths  ---------------*/

		for (length_count = 0; length_count < MPITEST_num_message_lengths(); length_count++)
		{
		    byte_length = MPITEST_get_message_length(length_count);

		    length = MPITEST_byte_to_element(test_type, byte_length);

		    sprintf(info_buf, "(%d,%d,%d) length %d commsize %d commtype %d data_type %d",
				length_count, comm_count, type_count, length,
				test_nump, comm_type, test_type);
		    if (MPITEST_current_rank == 0)
			MPITEST_message(MPITEST_INFO1, info_buf);


		    /*-------------------------------------------------
	                                   Recv
		         All nodes receive a message from MPI_PROC_NULL
		    -------------------------------------------------*/

		    loop_cnt++;
		    loop_fail=0;

		    ierr = MPI_Recv(recv_buffer, length,
				     MPITEST_mpi_datatypes[test_type],
				     MPI_PROC_NULL, 1, comm, &recv_stat);

		    if (ierr != MPI_SUCCESS)
		    {
		    	sprintf(info_buf, "(%d,%d,%d) length %d commsize %d commtype %d data_type %d",
				length_count, comm_count, type_count, length,
				test_nump, comm_type, test_type);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
			sprintf(info_buf, "Non-zero return code (%d) from MPI_Recv", ierr);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
			MPI_Error_string(ierr, error_string, &size);
			MPITEST_message(MPITEST_NONFATAL, error_string);
			loop_fail++;
		    }	/* Error Test  */

		    /*
		     * Call the MPI_Get_Count function, and
		     * compare value with length
		     */
		    cnt_len = -1;
		    ierr = MPI_Get_count(&recv_stat,
		        MPITEST_mpi_datatypes[test_type], &cnt_len);
		    if (ierr != MPI_SUCCESS)
		    {
			sprintf(info_buf, "Non-zero return code (%d) from MPI_Get_count", ierr);
		        MPITEST_message(MPITEST_NONFATAL, info_buf);
		        MPI_Error_string(ierr, error_string, &size);
		        MPITEST_message(MPITEST_NONFATAL, error_string);
		        loop_fail++;
		    }


		    /*
		     * Print non-fatal error if Received lenth
		     * not equal to 0
		     */
		    if (cnt_len != 0)
		    {
		        sprintf(info_buf, "Receive length = %d, expected 0",
				cnt_len);
		        MPITEST_message(MPITEST_NONFATAL, info_buf);
		        loop_fail++;
		    }

		    /*
		     * Print non-fatal error if tag is not MPI_ANY_TAG
		     */
		    if (recv_stat.MPI_TAG != MPI_ANY_TAG)
		    {
		        sprintf(info_buf, "Unexpected tag value=%d, expected MPI_ANY_TAG",
		        recv_stat.MPI_TAG);
		        MPITEST_message(MPITEST_NONFATAL, info_buf);
		        loop_fail++;
		    }
 
		    /*
		     * Print non-fatal error if source is not MPI_PROC_NULL
		     */
		    if (recv_stat.MPI_SOURCE != MPI_PROC_NULL)
		    {
		        sprintf(info_buf, "Unexpected source value=%d, expected MPI_PROC_NULL",
		        recv_stat.MPI_SOURCE);
		        MPITEST_message(MPITEST_NONFATAL, info_buf);
		        loop_fail++;
		    }

		    if (loop_fail != 0) fail++;


		}	/* Loop over Message Lengths  */

		free(recv_buffer);
	    }	/* Loop over Data Types  */

}	/* node rank not defined for this communicator */


	MPITEST_free_communicator(comm_type, &comm);
	/*-------------------------------------------------------------------*/

	/*
	 * With the current design of the program, we skip all the code for the
	 * nodes that are not members of the communicator, so this is the first
	 * point that we can place a barrier
	 */
	MPI_Barrier(MPI_COMM_WORLD);

	/*-------------------------------------------------------------------*/

    }	/* Loop over Communicators  */

    /* report overall results  */

    MPITEST_report(loop_cnt - fail, fail, 0, testname);

    MPI_Finalize();

    return fail;

}/* main() */
