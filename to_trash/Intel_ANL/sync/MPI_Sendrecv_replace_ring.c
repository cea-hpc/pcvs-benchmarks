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

               MPI_Sendrecv_replace funtional Ring test

Reference:  Mpi Standard, Section 3.10

Each node sends to the next higher numbered logical node, and receives from
the next lowered numbered logical node, with the node numbers wrapping around.

This test runs in any communicator/subcommunicator of two or more nodes, 
with any data type, and with any non-negative message length.

The MPITEST environment provides looping over communicator size and
type, message length, and data type.  The properties of the loops are
encoded in configuration arrays in the file mpitest_cfg.h .  See the
MPITEST README for further details.
-----------------------------------------------------------------------------
  NOTE !!!  6/96  
    On the Paragon the MPI_Sendrecv_replace code copies the message to a 
    temporary buffer, using MPI_Pack to circumvent data typing.  MPI_Pack
    adds a 16 byte (?) header to the message.  If you then call MPI_Get_count
    that header is reflected in the message length if.

****************************************************************************/
#include "mpitest_cfg.h"
#include "mpitest.h"


int main(int argc, char *argv[])
{
    int
        byte_length,	/* The length of the current buffer in bytes         */
        send_cnt_len,	/* received length returned by MPI_Get_Count         */
        recv_cnt_len,
        comm_count,	/* loop counter for communicator loop                */
        comm_index,	/* the array index of the current comm               */
        comm_type,	/* the index of the current communicator type        */
        my_comm_size,   /* number of nodes in subcommunicator                */
        remote_size,    /* number of nodes in remote subcommunicator         */
        isok,
        error,	        /* errors from one MPI call                          */
        fail,   	/* counts total number of failures                   */
        loop_fail,	/* counts number of failures in loop                 */
        ierr,   	/* return value from MPI calls                       */
        inter_comm,	/* Intercommunicator flag, true if intercommunicator */
        send_length,	/* The length of the current buffer                  */
        recv_length,	/* The length of the current buffer                  */
        length_count,   /* index for message length loop                     */
        loop_cnt,	/* counts total number of loops through test         */
        max_byte_length,/* maximum buffer length in bytes                    */
        max_length,	/* maximum buffer length specified in config. file   */
        ntimes,  	/* # of communicator groups INTRA=1, INTER=2         */
        print_flag,     /* node 0 of INTRA, or node 0 of left group of INTER */
        recv_from,      /* subcommunicator node to receive from              */
        send_to,	/* subcommunicator node to send to                   */
        size,   	/* return size from MPI_Error_string                 */
        test_nump,	/* The number of processors in current communicator  */
        test_type,	/* the index of the current buffer type              */
        type_count,	/* loop counter for data type loop                   */
       *ub,     	/* Pointer to value of maximum tag                   */
        found;  	/* boolean for MPI_Get_attr()			     */

    struct dataTemplate
        value;  	/* dataTemplate for initializing buffers             */

    void *send_buffer;	/* message buffer                                    */


    char
        info_buf[256],	/* buffer for passing mesages to MPITEST             */
        testname[64];	/* the name of this test                             */
    char error_string[MPI_MAX_ERROR_STRING];

    MPI_Comm
	comm,   	/* MPI communicator                                  */
	barrier_comm;	/* If intercommunicator, merged comm for Barrier     */

    MPI_Status
	stat;


    /*------------------------------  MPI_Init  -----------------------------*/
    ierr = MPI_Init(&argc, &argv);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr);
	MPITEST_message(MPITEST_FATAL, info_buf);
    }

    sprintf(testname, "MPI_Sendrecv_replace:  Ring Test");

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

    /*----------------------  Loop over Communicators  ----------------------*/

    for (comm_count = 0; comm_count < MPITEST_num_comm_sizes(); comm_count++)
    {
	comm_index = MPITEST_get_comm_index(comm_count);
	comm_type  = MPITEST_get_comm_type(comm_count);
	test_nump  = MPITEST_get_communicator(comm_type, comm_index, &comm);
	
	if (MPITEST_current_rank != MPI_UNDEFINED)
	{
    /*** Test if INTER-communicator:  inter_comm is true if this is an
	 inter-communicator                                               ***/

	MPI_Comm_test_inter(comm, &inter_comm);

	/* Skip everything if not a member of this communicator OR
	   there are < 2 nodes in this node's communicator/subcommunicator */
	isok = TRUE;
        my_comm_size = test_nump;
        if(inter_comm)
	  {
          /* for intercommunicator choose size of smallest subcommunicator */
	    MPI_Comm_size(comm,&my_comm_size);
            MPI_Comm_remote_size(comm, &remote_size);
            if ( remote_size < my_comm_size)
	      my_comm_size = remote_size;
	    if(MPITEST_current_rank >= my_comm_size)
	      isok = FALSE;
	  }
 	if (MPITEST_current_rank != MPI_UNDEFINED  && my_comm_size > 1 && isok)
	{

		    sprintf(info_buf, " Node:  %d  %6d  %6d %6d %6d %6d %6d %6d  %6d ", MPITEST_current_rank, MPITEST_me, test_nump, my_comm_size, inter_comm, comm_count, comm_index, comm_type, MPITEST_inter  );
			MPITEST_message(MPITEST_INFO1, info_buf);
	  if(MPITEST_current_rank == 0)
	         print_flag = 1;


	    /*----------------  Loop over Data Types  ----------------------*/

	    for (type_count = 0; type_count < MPITEST_num_datatypes(); type_count++)
	    {
		test_type = MPITEST_get_datatype(type_count);

		/* convert the number of bytes in the maximum length message */
		/* into the number of elements of the current type */

		max_length = MPITEST_byte_to_element(test_type, max_byte_length);

		/* Allocate send and receive Buffers */

		MPITEST_get_buffer(test_type, max_length, &send_buffer);

		/*-------------  Loop over Message Lengths  -----------------*/

		for (length_count = 0; length_count < MPITEST_num_message_lengths(); length_count++)
		{
		    byte_length = MPITEST_get_message_length(length_count);

		    send_length = MPITEST_byte_to_element(test_type, byte_length);

		    if ( print_flag)
		    {
			sprintf(info_buf, "(%d,%d,%d) recv_length %d commsize %d commtype %d data_type %d", length_count, comm_count, type_count, send_length, test_nump, comm_type, test_type);
			MPITEST_message(MPITEST_INFO1, info_buf);

		    }

		    /* Initialize the receive buffer to -1 */
		    MPITEST_dataTemplate_init(&value, -1);
		    MPITEST_init_buffer(test_type, send_length + 1, value, send_buffer);

		    MPITEST_dataTemplate_init(&value, MPITEST_current_rank);
		    MPITEST_init_buffer(test_type, send_length, value, send_buffer);

		    loop_cnt++;
		    loop_fail = 0;


		    recv_from = (my_comm_size + MPITEST_current_rank - 1) % my_comm_size;
                    send_to   = (            MPITEST_current_rank + 1) % my_comm_size;

		    sprintf(info_buf, " Me:  %d   loop_cnt:  %d   recv_from = %d   send_to = %d    send/recv length = %d/%d", MPITEST_current_rank, loop_cnt, recv_from, send_to, send_length, send_length  );
			MPITEST_message(MPITEST_INFO1, info_buf);

		    ierr = MPI_Sendrecv_replace(
					send_buffer,
					send_length,
					MPITEST_mpi_datatypes[test_type],
					send_to,
					MPITEST_current_rank,
					recv_from,
					recv_from,
					comm,
					&stat);


		    if (ierr != MPI_SUCCESS)
		    {
			sprintf(info_buf, "Non-zero return code (%d) from node %d  MPI_Sendrecv_replace", ierr, MPITEST_current_rank);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
			MPI_Error_string(ierr, error_string, &size);
			MPITEST_message(MPITEST_NONFATAL, error_string);
		    sprintf(info_buf, " NErr:  %d  %6d  %6d %6d %6d %6d %6d %6d %6d ", MPITEST_current_rank, MPITEST_me, test_nump, my_comm_size, inter_comm, comm_count, comm_index, comm_type, MPITEST_inter  );
			MPITEST_message(MPITEST_NONFATAL, info_buf);			loop_fail++;
		    sprintf(info_buf, " MeErr:  %d   loop_cnt:  %d   recv_from = %d   send_to = %d    send/recv length = %d/%d", MPITEST_current_rank, loop_cnt, recv_from, send_to, send_length, send_length  );
			MPITEST_message(MPITEST_NONFATAL, info_buf);
		    }	/* End of node Sendrecv_replace Error Test  */
		    /*
		     * Set up the dataTemplate for checking the nodes recv'd
		     * buffer.
		     */

		    MPITEST_dataTemplate_init(&value, recv_from);
		    error = MPITEST_buffer_errors(test_type, send_length, value, send_buffer);

		    /* check for receive buffer overflow */
		    MPITEST_dataTemplate_init(&value, -1);
		    error += MPITEST_buffer_errors_ov(test_type, send_length, value, send_buffer);

		    if (error)
		    {
			sprintf(info_buf, "%d errors in buffer (%d,%d,%d) len %d commsize %d commtype %d data_type %d sender %d", error, length_count, comm_count, type_count, send_length, my_comm_size, comm_type, test_type, recv_from);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
			loop_fail++;
		    }
		    else
		    {
			sprintf(info_buf, "%d errors found in buffer", error);
			MPITEST_message(MPITEST_INFO2, info_buf);
		    }
		    /*
		     * Call the MPI_Get_Count function, and compare value
		     * with length
		     */
		    recv_cnt_len = -1;

		    ierr = MPI_Get_count(&stat,
			   MPITEST_mpi_datatypes[test_type], &recv_cnt_len);
		    if (ierr != MPI_SUCCESS)
		    {
			sprintf(info_buf, "Non-zero return code (%d) from MPI_Get_count", ierr);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
			MPI_Error_string(ierr, error_string, &size);
			MPITEST_message(MPITEST_NONFATAL, error_string);
			loop_fail++;
		    }
		    /*
		     * Print non-fatal error if Received length not equal to
		     * send length
		     */

		    error = send_length - recv_cnt_len;

		    if (error)
		    {
			sprintf(info_buf, "Send/Receive lengths differ - Sender(node/length)=%d/%d,  Receiver(node/length)=%d/%d", stat.MPI_SOURCE, send_length, MPITEST_current_rank, recv_cnt_len);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
			loop_fail++;
		    }
		    /*
		     * Print non-fatal error if tag is not correct.
		     */
		    if (stat.MPI_TAG != recv_from)
		    {
			sprintf(info_buf, "Unexpected tag value=%d, expected=%d",stat.MPI_TAG, recv_from);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
			loop_fail++;
		    }
		    /*
		     * Print non-fatal error if source is not correct.
		     */
		    if (stat.MPI_SOURCE != recv_from)
		    {
			sprintf(info_buf, "Unexpected source value=%d, expected=%d", stat.MPI_SOURCE, recv_from);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
			loop_fail++;
		    }

		    if (loop_fail != 0)
			fail++;

}/* Loop over Message Lengths  */

free(send_buffer);

}/* Loop over Data Types  */

}/* node rank not defined for this communicator */

}
MPITEST_free_communicator(comm_type, &comm);

}/* Loop over Communicators  */
  MPI_Barrier(MPI_COMM_WORLD);
/* report overall results  */

MPITEST_report(loop_cnt - fail, fail, 0, testname);

MPI_Finalize();

return fail;

}/* main() */
