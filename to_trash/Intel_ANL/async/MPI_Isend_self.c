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
		  Test for   MPI_Isend() to self
		       with  MPI_Recv()  to receive messages 
This tests the nonblocking MPI_Isend operation from a node to itself.
A quality of implementation test, since this relies on message buffering
not a part of the MPI spec.  However, this is useful for many applications.

To avoid hangs, rank 0 waits for a message from rank 1, and aborts the test
after 5 minutes if it has not been notified of success.

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
        comm_count,	    /* loop counter for communicator loop            */
        comm_index,	    /* the array index of the current comm           */
        comm_type,	    /* the index of the current communicator type    */
        error,    	    /* errors from one MPI call                      */
        fail,   	    /* counts total number of failures               */
        i, j,	            /* utility loop index variables                  */
        ierr,	            /* return value from MPI calls                   */
        length,	            /* The length of the current buffer              */
        length_count,	    /* loop counter for message length loop          */
        loop_cnt,	    /* counts total number of loops through test     */
        loop_fail,	    /* counts number of failures in loop             */
        max_byte_length,    /* maximum buffer length in bytes                */
        max_length,	    /* max buffer length specified in config. file   */
        size,	            /* return size from MPI_Error_string             */
        test_nump,	    /* The number of nodes in current communicator   */
        test_type,	    /* the index of the current buffer type          */
        type_count,	    /* loop counter for data type loop               */
        cnt_len,            /* length of received message                    */
	flag,               /* indicates Irecv() completed                   */
	irecv_buff;         /* buffer for Irecv()                            */

        void *send_buffer;  /* message buffer                                */
        void *recv_buffer;  /* message buffer                                */

        char
	  info_buf[256],    /* buffer for passing mesages to MPITEST         */
	  testname[64];     /* the name of this test                         */
    char error_string[MPI_MAX_ERROR_STRING];

        MPI_Comm
	  comm;	            /* MPI communicator                              */

        MPI_Status
	  recv_stat,	    /* MPI  status structure                         */
	  send_stat;

        MPI_Request
	  recv_req,         /*  MPI request structure                        */
	  send_request;

        struct dataTemplate
          value;

    /*-----------------------------  MPI_Init  ------------------------------*/
    ierr = MPI_Init(&argc, &argv);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr);
	MPITEST_message(MPITEST_FATAL, info_buf);
    }


    sprintf(testname, "MPI_Isend_self: all Isend to SELF");

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
	if ((MPITEST_current_rank != MPI_UNDEFINED) &&
            (MPITEST_inter == MPITEST_NOT_INTER))
	{
	    if (MPITEST_current_rank == 0)
	    {
		if (test_nump > 1)
		{
		    /* Node 1 will send a message when completed;
                     * wait for it, and fail after 30 seconds
		    */
		    ierr = MPI_Irecv(&irecv_buff, 0,
			     MPI_INT, 1, 1, comm, &recv_req);

		    if (ierr != MPI_SUCCESS)
		    {
			sprintf(info_buf, "Non-zero return code (%d) from MPI_Irecv", ierr);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
			MPI_Error_string(ierr, error_string, &size);
			MPITEST_message(MPITEST_FATAL, error_string);
		    }	/* Error Test  */

		    flag = 0;
		    loop_fail = 0;
		    while (flag == 0)
		    {
			loop_fail++;
			ierr = MPI_Test(&recv_req, &flag, &recv_stat);
			if (ierr != MPI_SUCCESS)
			{
			  sprintf(info_buf, "Non-zero return code (%d) from MPI_Test", ierr);
			  MPITEST_message(MPITEST_NONFATAL, info_buf);
			  MPI_Error_string(ierr, error_string, &size);
			  MPITEST_message(MPITEST_FATAL, error_string);
			}	/* Error Test  */
			if (flag == 0) sleep(1);
			if (loop_fail > 300)
			{
			  sprintf(info_buf, "Nodes sending to self not responding, test FAILED");
			  MPITEST_message(MPITEST_FATAL, info_buf);
			}
		    } /* while */

		}
	    } /* rank 0 waits for message */
	    else
	    { /* other ranks do test */

	    /*------------------  Loop over Data Types  ---------------------*/

		for (type_count = 0; type_count < MPITEST_num_datatypes(); type_count++)
		{
		    test_type = MPITEST_get_datatype(type_count);

		    /* convert the number of bytes in the max length message */
		    /* into the number of elements of the current type */

		    max_length = MPITEST_byte_to_element(test_type, max_byte_length);

		    /* Allocate send buffer */
		    MPITEST_get_buffer(test_type, max_length, &send_buffer);
		    MPITEST_get_buffer(test_type, max_length, &recv_buffer);

		    /*-------------  Loop over Message Lengths  ---------------*/

		    for (length_count = 0; length_count < MPITEST_num_message_lengths(); length_count++)
		    {
		        byte_length = MPITEST_get_message_length(length_count);

		        length = MPITEST_byte_to_element(test_type, byte_length);

		        sprintf(info_buf, "(%d,%d,%d) length %d commsize %d commtype %d data_type %d",
				length_count, comm_count, type_count, length,
				test_nump, comm_type, test_type);

		        /*-------------------------------------------------
	                                   Isend
		             All nodes send a message to self
		        -------------------------------------------------*/

			loop_cnt++;
			loop_fail=0;

			MPITEST_dataTemplate_init(&value, MPITEST_current_rank);
			MPITEST_init_buffer(test_type, length + 1,
					    value, send_buffer);

			ierr = MPI_Isend(send_buffer, length,
				     MPITEST_mpi_datatypes[test_type],
				     MPITEST_current_rank, 1, comm,
				     &send_request);

			if (ierr != MPI_SUCCESS)
			{
			    sprintf(info_buf, "(%d,%d,%d) length %d commsize %d commtype %d data_type %d",
				length_count, comm_count, type_count, length,
				test_nump, comm_type, test_type);
			    MPITEST_message(MPITEST_NONFATAL, info_buf);
			    sprintf(info_buf, "Non-zero return code (%d) from MPI_Isend", ierr);
			    MPITEST_message(MPITEST_NONFATAL, info_buf);
			    MPI_Error_string(ierr, error_string, &size);
			    MPITEST_message(MPITEST_NONFATAL, error_string);
			    loop_fail++;
			}	/* Isend Error Test  */


			MPITEST_dataTemplate_init(&value, -1);
			MPITEST_init_buffer(test_type, length + 1,
					    value, recv_buffer);

			ierr = MPI_Recv(recv_buffer, 
					length,
					MPITEST_mpi_datatypes[test_type],
					MPITEST_current_rank, 
					1, 
					comm, 
					&recv_stat);

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
			 * Set up the dataTemplate for checking the
			 * recv'd buffer.  Note that the sending node
			 * rank will be sent.
			 */

			MPITEST_dataTemplate_init(&value, MPITEST_current_rank);
			error = MPITEST_buffer_errors(test_type,
					length, value, recv_buffer);

			/* check for receive buffer overflow */
			MPITEST_dataTemplate_init(&value, -1);
			error += MPITEST_buffer_errors_ov(test_type,
			                  length, value, recv_buffer);

			if (error)
			{
			    sprintf(info_buf, "%d errors in buffer (%d,%d,%d) len %d commsize %d commtype %d data_type %d",
			            error, length_count, comm_count,
			            type_count, length, test_nump,
			            comm_type, test_type);
			    loop_fail++;
			    MPITEST_message(MPITEST_NONFATAL, info_buf);
			}
			else
			{
			    sprintf(info_buf, "%d errors found in buffer", error);
			    MPITEST_message(MPITEST_INFO2, info_buf);
			}

			/*
			 * Call the MPI_Get_Count function, and compare
			 * value with length
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
			 * Print non-fatal error if Received length not
			 * equal to send length
			 */
			error = length - cnt_len;
			if (error)
			{
			    sprintf(info_buf, "Send/Receive lengths differ - Sender(node/length)=%d/%d,  Receiver(node/length)=%d/%d",
			        recv_stat.MPI_SOURCE, length,
			        MPITEST_current_rank, cnt_len);
			    MPITEST_message(MPITEST_NONFATAL, info_buf);
			    loop_fail++;
			}

			/*
			 * Print non-fatal error if tag is not
			 * correct.
			 */
			if (recv_stat.MPI_TAG != 1)
			{
			    sprintf(info_buf, "Unexpected tag value=%d, expected=%d",
			        recv_stat.MPI_TAG, 1);
			    MPITEST_message(MPITEST_NONFATAL, info_buf);
			    loop_fail++;
			}

			/*
			 * Print non-fatal error if source is not
			 * correct.
			 */
			if (recv_stat.MPI_SOURCE != MPITEST_current_rank)
			{
			    sprintf(info_buf, "Unexpected source value=%d, expected=%d",
			        recv_stat.MPI_SOURCE, MPITEST_current_rank);
			    MPITEST_message(MPITEST_NONFATAL, info_buf);
			    loop_fail++;
			}

                        ierr = MPI_Wait(&send_request, &send_stat);
		        if (ierr != MPI_SUCCESS)
			{
			    sprintf(info_buf, "Non-zero return code (%d) from MPI_Wait on Isend", ierr);
			    MPITEST_message(MPITEST_NONFATAL, info_buf);
			    MPI_Error_string(ierr, error_string, &size);
			    MPITEST_message(MPITEST_NONFATAL, error_string);
			    loop_fail++;
			}      /*  Error:  Wait on Isend  */


			if (loop_fail != 0) fail++;

		    }	/* Loop over Message Lengths  */

		    free(send_buffer);
		    free(recv_buffer);
		}	/* Loop over Data Types  */
		if (MPITEST_current_rank == 1)
		{
		    ierr = MPI_Isend(send_buffer, 0, MPI_INT, 0, 1, comm, &send_request);

		    if (ierr != MPI_SUCCESS)
		    {
			sprintf(info_buf, "(%d,%d,%d) length %d commsize %d commtype %d data_type %d",
			length_count, comm_count, type_count, length,
			test_nump, comm_type, test_type);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
			sprintf(info_buf, "Non-zero return code (%d) from MPI_Isend signal", ierr);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
			MPI_Error_string(ierr, error_string, &size);
			MPITEST_message(MPITEST_FATAL, error_string);
		    }	/* Error Test  */


                    ierr = MPI_Wait(&send_request, &send_stat);

		    if (ierr != MPI_SUCCESS)
	     	    {
			sprintf(info_buf, "Non-zero return code (%d) from MPI_Wait on Isend", ierr);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
			MPI_Error_string(ierr, error_string, &size);
			MPITEST_message(MPITEST_NONFATAL, error_string);
			loop_fail++;
		    }  /*  Error:  Wait on Isend  */
	
		} /* rank 1 signals rank 0 */
	    } /* not rank 0 */

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
