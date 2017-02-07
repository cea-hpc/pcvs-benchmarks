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
	  Test for MPI_Isend()/MPI_Irecv; flood one node with messages

This tests the  nonblocking MPI_Isend operation, with one node flooding
another with messages before they are received.  This tests the buffering
of Isends and Ireceives (if any), and the ability to recover if the buffers
are filled.

This test requires 3 ranks.  Rank 1 sends many large messages to rank 2
as fast as it can, and sends a message to rank 0 as well.  Rank 2
sleeps for 90 seconds, then begins to read and validate the messages
from rank 1.

Rank 0 monitors the messages from rank 1.  When rank 0 detects a delay in
the sending of messages, this indicates that the message buffer has been
exceeded, and prints an informational message telling how many messages
between rank 1 and 2 were apparently buffered.
------------------------------------------------------------------------------
NOTES:
      This nonblocking version differs from the blocking version only in
      that it replaces the Send with Isend.  The program still has both an
      Irecv  and a Recv.
******************************************************************************/

#include "mpitest_cfg.h"
#include "mpitest.h"

#if defined(HAVE_UNISTD_H)
#  include <unistd.h>
#endif

#define LOOP_SIZE 5000

int main(int argc, char *argv[])
{

    int
        byte_length,	    /* The length of the current buffer in bytes     */
        error,    	    /* errors from one MPI call                      */
        fail,   	    /* counts total number of failures               */
        verify, 	    /* recommend manually verify result from rank 0  */
        i, j,	            /* utility loop index variables                  */
        ierr,	            /* return value from MPI calls                   */
        length_count,	    /* loop counter for message length loop          */
        loop_cnt,	    /* counts total number of loops through test     */
        loop_fail,	    /* counts number of failures in loop             */
        max_byte_length,    /* maximum buffer length in bytes                */
        max_length,	    /* max buffer length specified in config. file   */
        size,	            /* return size from MPI_Error_string             */
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

	MPI_Request
	  recv_req,
          recv_request,
          send_request;

	MPI_Status
          send_stat,
	  recv_stat;

        struct dataTemplate
          value;

    /*-----------------------------  MPI_Init  ------------------------------*/
    ierr = MPI_Init(&argc, &argv);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr);
	MPITEST_message(MPITEST_FATAL, info_buf);
    }


    sprintf(testname, "MPI_Isend_flood");

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

    max_byte_length = MPITEST_get_max_message_length();
    MPITEST_types[0] = MPITEST_char;
    type_count = 0;
    test_type = MPITEST_get_datatype(type_count);
    max_length = MPITEST_byte_to_element(test_type, max_byte_length);
    if (max_length < 2048) max_length = 2048;

    if (MPITEST_nump < 3)
    {
	MPITEST_message(MPITEST_INFO0, "This test requires at least 3 ranks");
	MPITEST_report(0, 0, 1, testname);
	MPI_Finalize();
	return 0;
    }

    if (MPITEST_me == 0)
    {
	/* Rank 1 will send a message when when every send is completed.
	 * After 30 seconds with no message, this indicates the message
	 * buffer is full, so print a message.
	*/
	verify = 1;
	for (loop_cnt = 0; loop_cnt < LOOP_SIZE; loop_cnt++)
	{
	    ierr = MPI_Irecv(&irecv_buff, 0,
		 MPI_INT, 1, 1, MPI_COMM_WORLD, &recv_req);

	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Irecv", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
	    }	/* Error Test  */

	    flag = 0;
	    loop_fail = 0;
	    while (loop_fail < LOOP_SIZE)
	    {
		ierr = MPI_Test(&recv_req, &flag, &recv_stat);
		if (ierr != MPI_SUCCESS)
		{
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_Test", ierr);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_FATAL, error_string);
		}	/* Error Test  */
/* 		if ((flag == 0) && (loop_fail > 15)) */
/* 		    sleep(1); */
		if (flag == 1)
		    loop_fail = LOOP_SIZE;
		if (loop_fail == 45)
		{
		    sprintf(info_buf, "Rank 1 sent %d mesasages containing %d characters before filling buffer on node 2", loop_cnt, max_length);
		    MPITEST_message(MPITEST_INFO0, info_buf);
		}
	    loop_fail++;
	} /* while */

    } /* for */

    } /* rank 0 waits for message */

    else if (MPITEST_me <= 2)
    {
	/* Allocate send buffer */
	MPITEST_get_buffer(test_type, max_length, &send_buffer);
	MPITEST_get_buffer(test_type, max_length, &recv_buffer);

	if (MPITEST_me == 1)
	{ /* rank 1 does sends */
	    /*-------------------------------------------------
	                       Send
	           Rank 0 sends messages to rank 1
	    -------------------------------------------------*/

	    loop_fail = 0;

	    MPITEST_dataTemplate_init(&value, MPITEST_me);
	    MPITEST_init_buffer_inc(test_type, max_length,
	    		    value, send_buffer);

	    for (loop_cnt = 0; loop_cnt < LOOP_SIZE; loop_cnt++)
	    {
		/* Send a large message to rank 2 */

		ierr = MPI_Isend(send_buffer, 
				max_length,
				MPITEST_mpi_datatypes[test_type],
				2, 
				loop_cnt, 
				MPI_COMM_WORLD,
				&send_request);

		if (ierr != MPI_SUCCESS)
		{
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_Isend", ierr);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_FATAL, error_string);
		    loop_fail++;
		}	/* Error Test  */

                ierr = MPI_Wait(&send_request, &send_stat);

	        if (ierr != MPI_SUCCESS)
		{
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_Wait on Isend", ierr);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
	       	    MPI_Error_string(ierr, error_string, &size);
       		    MPITEST_message(MPITEST_NONFATAL, error_string);
		    loop_fail++;
	       	}	/* End of Wait Error Test  */


		/* Send a signal message to rank 0 */
		ierr = MPI_Isend(send_buffer, 
				 0, 
				 MPI_INT, 
				 0, 
				 1, 
				 MPI_COMM_WORLD,
				 &send_request);
		if (ierr != MPI_SUCCESS)
		{
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
	       	}	/* End of Wait Error Test  */


	    if (loop_fail != 0) fail++;
	    } /* for */

	} /* rank 1 sends */
	else
	if (MPITEST_me == 2){ /* rank 2 does receives */
	    /* Let messages pile up for a while */
	    sleep(90);

	    for (loop_cnt = 0; loop_cnt < LOOP_SIZE; loop_cnt++)
	    {
		loop_fail = 0;
		MPITEST_dataTemplate_init(&value, -1);
		MPITEST_init_buffer(test_type, max_length + 1,
			    value, recv_buffer);

		ierr = MPI_Recv(recv_buffer,
				max_length,
				MPITEST_mpi_datatypes[test_type],
				1, 
				loop_cnt, 
				MPI_COMM_WORLD, 
				&recv_stat);

		if (ierr != MPI_SUCCESS)
		{
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_Recv", ierr);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_NONFATAL, error_string);
		    loop_fail++;
		}	/* Error Test  */


		/*
		 * Set up the dataTemplate for checking the
		 * recv'd buffer.
		 */
		MPITEST_dataTemplate_init(&value, 1);
		error = MPITEST_buffer_errors_inc(test_type,
	    		max_length, value, recv_buffer);

		/* check for receive buffer overflow */
		MPITEST_dataTemplate_init(&value, -1);
		error += MPITEST_buffer_errors_ov(test_type,
			      max_length, value, recv_buffer);

		if (error)
		{
		    sprintf(info_buf, "%d errors in received buffer", error);
		    loop_fail++;
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
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
		error = max_length - cnt_len;
		if (error)
		{
		    sprintf(info_buf, "Send/Receive lengths differ - Sender(node/length)=%d/%d,  Receiver(node/length)=%d/%d",
			recv_stat.MPI_SOURCE, max_length,
			MPITEST_me, cnt_len);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    loop_fail++;
		}

		/*
		 * Print non-fatal error if tag is not
		 * correct.
		 */
		if (recv_stat.MPI_TAG != loop_cnt)
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
		if (recv_stat.MPI_SOURCE != 1)
		{
		    sprintf(info_buf, "Unexpected source value=%d, expected=%d",
			recv_stat.MPI_SOURCE, MPITEST_me);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    loop_fail++;
		}

		if (loop_fail != 0) fail++;
	    }
	}
	free(send_buffer);
	free(recv_buffer);
    } /* not rank 0 */

    /* report overall results  */
    MPITEST_report(loop_cnt - fail, fail, verify, testname);

    MPI_Finalize();

    return fail;

}/* main() */
