/*-----------------------------------------------------------------------------
MESSAGE PASSING INTERFACE TEST CASE SUITE

Copyright - 1996 Intel Corporation

Intel Corporation hereby grants a non-exclusive license under Intel's copyright
to copy, modify and distribute this software for any purpose and without
fee, provided that the above copyright notice and the following paragraphs
appear on all copies.

Intel Corporation makes no representation that the test cases comprising this
suite are correct or are an accurate representation of any standard.

IN NO EVENT SHALL INTEL HAVE ANY LIABILITY FOR ANY DIRECT, INDIRECT OR
SPECULATIVE DAMAGES, (INCLUDING WITHOUT LIMITING THE FOREGOING,
CONSEQUENTIAL, INCIDENTAL AND SPECIAL DAMAGES) INCLUDING, BUT NOT LIMITED TO
INFRINGEMENT, LOSS OF USE, BUSINESS INTERRUPTIONS, AND LOSS OF PROFITS,
IRRESPECTIVE OF WHETHER INTEL HAS ADVANCE NOTICE OF THE POSSIBILITY OF ANY
SUCH DAMAGES.

INTEL CORPORATION SPECIFICALLY DISCLAIMS ANY WARRANTIES INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
PARTICULAR PURPOSE AND NON-INFRINGEMENT.  THE SOFTWARE PROVIDED HEREUNDER IS
ON AN "AS IS" BASIS AND INTEL CORPORATION HAS NO OBLIGATION TO PROVIDE
MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS OR MODIFICATIONS.
-----------------------------------------------------------------------------*/
/******************************************************************************
		  Test for MPI_Ibsend():  ALL Ibsend to ROOT
                  and MPI_Irecv(): Any source/tag specified

Ibsend Inner buffering, SEND before RECEIVE, ALL-to-ROOT.

Tests the nonblocking buffered MPI_Ibsend and MPI_Irecv operations.
MPI_Test, and MPI_Get_count are also used in conjunction with the 
nonblocking I/O calls.

MPI_Standard:  Sections  3.7, 3.7.1,  3.7.2. 3.7.3
USING MPI:     Sections  4.4, 7.9

Ibsend is a Nonblocking (asynchronous) send.  Ibsend initiates the data 
transfer and returns control to the user.  Ibsend is, then, always a local
call.  A separate call, MPI_Wait or MPI_Test, is used to verify that the 
Ibsend has completed (data has been copied out of the send buffer). 

Irecv is, likewise, a nonblocking receive, and returns control to the user
as soon as the system is able to start writing data to the receive buffer.
An MPI_Wait or MPI_Test  call is used with the MPI_Ibsend, and MPI_Irecv to 
verify that the operation has properly completed.

Nonblocking communications use opaque request objects [ MPI Standard 3.7.1]
to match the operation that initiates the communication with the operation
that terminates it.  These request objects are accessed via a handle

For a single, INTRA-communicator, the program selects each node, in turn,
of the communicator to be the root.  the ROOT receives from each of the nodes.

Each Ibsend receives from  all other nodes, including itself.
Since MPI_Recv nonbloks until, at least, the send has been started, the program
has to post the sends before posting the receives.  The buffer space is
allocated dynamically based on the maximum message length, and the number of
nodes.  INNER buffering means that the program allocates the buffer for the
Ibsend seperately for each root.  After the root is chosen, the buffer is
allocated, the messages are send, and the buffer is deallocated.

For an INTER-communicator, there are two groups of nodes; each group being
termed a sub-communicator.  The nodes in each sub-communicator are numbered
 0 to (k-1) for a k-node sub-communicator.  So,the MPITEST_current_rank will
return duplicate node numbers for nodes (0 to k-1) where k is the number of
nodes in the smaller communicator.

The program cycles through the nodes in one sub-communicator, with each
selected node receiving from all of the nodes in the other sub-communicator.
Then the program reverses the send and receive role of the
two sub-communicators, and repeats the process.

The MPI_Ibsend function sends a message from one node to each other
node in a communicator.  The receiving node is referred to as the
"root" node.  Each node gets to be the root node, and receive from
all nodes.

This test initializes the send buffer with the root's rank in the
communicator (or an appropriate value for the non-integer types.)
Once the receiving nodes have completed their message receive, they check to
make sure the current root's rank is in the received buffer.

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
        buffsize,	    /* Size of malloc buffer for Ibsend              */
        berr,	            /* error flag for buffer ATTACH and DETACH       */
        byte_length,	    /* The length of the current buffer in bytes     */
        cnt_len,	    /* received length returned by MPI_Get_Count     */
        comm_count,	    /* loop counter for communicator loop            */
        comm_index,	    /* the array index of the current comm           */
        comm_type,	    /* the index of the current communicator type    */
        error,    	    /* errors from one MPI call                      */
        fail,   	    /* counts total number of failures               */
        loop_fail,   	    /* counts number of failures in loop             */
        get_from,	    /* Number of nodes sending current message       */
        grp_lup,	    /* index for comm groups (1 to ntimes)           */
        i, j,	            /* utility loop index variables                  */
        ierr,	            /* return value from MPI calls                   */
        inter_comm,	    /* flag, true, if intercommunicator              */
        left_recv_size,     /* Left Intercommunictor receive size            */
        left_send_size,	    /* Left Intercommunicator send size              */
        length,	            /* The length of the current buffer              */
        length_count,	    /* loop counter for message length loop          */
        loop_cnt,	    /* counts total number of loops through test     */
        max_byte_length,    /* maximum buffer length in bytes                */
        max_length,	    /* max buffer length specified in config. file   */
        ntimes,             /* # of communicator groups INTRA=1, INTER=2     */
        print_node_flag,    /* node 0 of INTRA, node 0 of left comm of INTER */
        receivers,	    /* INTER-comm #receivers for this send/rec choice*/
        recv_group,	    /* INTER-comm test receiving group(0 or 1)       */
        recv_size,	    /* INTER  receive size MPI_Comm_remote_size      */
        right_recv_size,    /* INTER: absolute count of right receive * nodes*/
        right_send_size,    /* INTER: count of right sending nodes           */
        root,	            /* the root of the current broadcast             */
        send_group,	    /* INTER-comm test sending group #(0 or 1)       */
        send_size,	    /* INTER: relative send size from MPI_Comm_size  */
        senders,	    /* INTER-comm #senders for this send/rec choice  */
        size,	            /* return size from MPI_Error_string             */
        test_nump,	    /* The number of nodes in current communicator   */
        test_type,	    /* the index of the current buffer type          */
        type_count;	    /* loop counter for data type loop               */

        struct dataTemplate
	  value;	    /* dataTemplate for initializing buffers         */

        void *recv_buffer;
        void *send_buffer;  /* message buffer                                */

        char
	  info_buf[256],    /* buffer for passing mesages to MPITEST         */
	  testname[64];     /* the name of this test                         */
    char error_string[MPI_MAX_ERROR_STRING];

        MPI_Comm
	  comm,	            /* MPI communicator                              */
          barrier_comm;     /* If intercommunicator, merged comm for Barrier */


        MPI_Status
	  recv_stat,	    /* MPI  status structure                         */
	  send_stat;

        MPI_Request
	  recv_request,     /*  MPI request structure                        */
	  send_request;

        char *buff;	    /* Ibsend message buffer                         */
        int bsize;	    /* Ibsend message buffer size                    */


    /*-----------------------------  MPI_Init  ------------------------------*/
    cnt_len = -1;

    ierr = MPI_Init(&argc, &argv);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr);
	MPITEST_message(MPITEST_FATAL, info_buf);
    }

    sprintf(testname, "MPI_Ibsend_ator: All Ibsend TO Root");

    /*-----------------------------  MPITEST_init  --------------------------*/
    MPITEST_init(argc, argv);
    /*
     * Set the print_node_flag: This is the flag that tells which node gets
     * to print.  For INTRA-communition it is simply node 0.  For
     * INTER-communication it is node zero of the left group.
     */

    print_node_flag = 0;
    if (MPITEST_me == 0)
	print_node_flag = 1;

    if (print_node_flag)
    {
	sprintf(info_buf, "Starting %s test", testname);
	MPITEST_message(MPITEST_INFO0, info_buf);
    }

    /* set the global error counter */
    fail = 0;
    loop_cnt = 0;

    max_byte_length = MPITEST_get_max_message_length();


    /*--------------------- Allocate buffer for Ibsend -----------------------*/

    buffsize = (2 * (MPI_BSEND_OVERHEAD + max_byte_length + 1000));
    buff = malloc(buffsize * sizeof(char));
    if(!buff)
    {
	sprintf(info_buf, "Ibsend malloc request failed");
	MPITEST_message(MPITEST_FATAL, info_buf);
    }
    berr = MPI_Buffer_attach(buff, buffsize);
    if (berr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Buffer_attach", berr);
	MPITEST_message(MPITEST_FATAL, info_buf);
    }

    /*  Detach, and get buffer pointer, and size for future attaches */

    berr = MPI_Buffer_detach(&buff, &bsize);

    if (berr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Buffer_detach", berr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(berr, error_string, &size);
	MPITEST_message(MPITEST_FATAL, error_string);
    }	/* Error Test  */


    /*-----------------------  Loop over Communicators  ---------------------*/

    for (comm_count = 0; comm_count < MPITEST_num_comm_sizes(); comm_count++)
    {
	comm_index = MPITEST_get_comm_index(comm_count);
	comm_type = MPITEST_get_comm_type(comm_count);

	/*
	 * Reset a bunch of variables that will be set when we get our
	 * communicator. These are mainly variables for determining
	 * sub-communicator, left or right, when we get an INTER-communicator.
	 */

	left_send_size = -1;
	left_recv_size = -1;
	right_send_size = -1;
	right_recv_size = -1;
	send_size = -1;
	recv_size = -1;

	inter_comm = -1;
	print_node_flag = 0;

	test_nump = MPITEST_get_communicator(comm_type, comm_index, &comm);

	/* Skip everything if not a member of this communicator */
	if (MPITEST_current_rank != MPI_UNDEFINED)
	{

	    /*
	     * Test if INTER-communicator:  inter_comm is true if this is an
	     * inter-communicator
	     */

	    MPI_Comm_test_inter(comm, &inter_comm);

	    /*
	     * Variables, send_size, and recv_size  both default to the total
	     * nodes in the communicator for an INTRA-communicator For an
	     * INTER-communicator: MPI_Comm_size returns the size of the
	     * communicator for all of the nodes in each respective communicator
	     * MPI_Comm_remote_size returns the size of the opposite
	     * communicator for all nodes in each respective communicator
	     */

	    if (inter_comm)
	    {	/* Do this stuff for INTER-communicators */

	        if (MPITEST_inter == 0)
	        {   /* Do this stuff for left group of an INTER-communicator */
		    MPI_Comm_size(comm, &send_size);
		    MPI_Comm_remote_size(comm, &recv_size);

		    left_send_size = send_size;
		    left_recv_size = recv_size;
		    right_send_size = recv_size;
		    right_recv_size = send_size;

		    if (MPITEST_current_rank == 0)
		    {	/* Do this stuff for node zero of left bank of
			 * INTER-communicator */
		        print_node_flag = 1;
		    }	/* End of INTER-communicator left group node zero */

		    ierr = MPI_Intercomm_merge(comm, FALSE, &barrier_comm);
		    if (ierr != MPI_SUCCESS)
		    {
			sprintf(info_buf, "Non-zero return code (%d) from MPI_Intercomm_merge", ierr);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
			MPI_Error_string(ierr, error_string, &size);
			MPITEST_message(MPITEST_FATAL, error_string);
		    }

	        }	/* End of      INTER-communicator left group */

	        if (MPITEST_inter == 1)
	        {	/* Do this for right group of an INTER-communicator  */
		    MPI_Comm_size(comm, &send_size);
		    MPI_Comm_remote_size(comm, &recv_size);

		    left_send_size = recv_size;
		    left_recv_size = send_size;
		    right_send_size = send_size;
		    right_recv_size = recv_size;

		    ierr = MPI_Intercomm_merge(comm, FALSE, &barrier_comm);
		    if (ierr != MPI_SUCCESS)
		    {
			sprintf(info_buf, "Non-zero return code (%d) from MPI_Intercomm_merge", ierr);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
			MPI_Error_string(ierr, error_string, &size);
			MPITEST_message(MPITEST_FATAL, error_string);
		    }

	        }	/* End of    INTER_communicator right group */
	    }	/* End of        INTER_communicator  */

	    /* Do stuff for INTRA-communicator  */
	    if (!inter_comm)
	    {
	        if (MPITEST_current_rank == 0) print_node_flag = 1;
		barrier_comm = comm;
	    }

	    /*-------------------  Loop over Data Types  --------------------*/

	    for (type_count = 0; type_count < MPITEST_num_datatypes(); type_count++)
	    {
		test_type = MPITEST_get_datatype(type_count);

		/* convert the number of bytes in the maximum length message */
		/* into the number of elements of the current type */

		max_length = MPITEST_byte_to_element(test_type, max_byte_length);

		/* Allocate send and receive Buffers */
		MPITEST_get_buffer(test_type, max_length, &recv_buffer);
		MPITEST_get_buffer(test_type, max_length+1, &send_buffer);

		/*---------------  Loop over Message Lengths  ---------------*/

		for (length_count = 0; length_count < MPITEST_num_message_lengths(); length_count++)
		{
		    byte_length = MPITEST_get_message_length(length_count);

		    length = MPITEST_byte_to_element(test_type, byte_length);

		    /*----------------  Loop over roots  --------------------*/

		    /*
		     * In an intra-communicator setting, we loop over the
		     * roots, and for each root each of the other nodes sends
		     * to that root.  For an inter-communicator setting, we
		     * have two sets of nodes, left and right, and we want to
		     * loop through all combinations.
		     */

		    i = -1;
		    j = -1;

		    /*
		     * This is an extra loop for INTER-communicators to have
		     * each group send and receive
		     */
		    ntimes = 1;
		    if (inter_comm)
			ntimes = 2;

		    /*
		     * For grp_lup = 1  send from left  sub-communicator  to
		     * right sub-communicator
		     */

		    /*
		     * For grp_lup = 2  send from right sub-communicator  to
		     * left sub-communicator
		     */

		    for (grp_lup = 1; grp_lup <= ntimes; grp_lup++)
		    {
			send_group = grp_lup - 1;
			recv_group = 2 - grp_lup;

			/* Set up the number of receivers  */

			receivers = test_nump;
			if (inter_comm)
			{
			    receivers = left_recv_size;
			    senders = left_send_size;
			}
			if (inter_comm && grp_lup == 2)
			{
			    receivers = right_recv_size;
			    senders = right_send_size;
			}
			for (root = 0; root < receivers; root++)
			{

			    /* print an informational message */
			    if (print_node_flag)
			    {
			      sprintf(info_buf, "(%d,%d,%d) length %d commsize %d commtype %d data_type %d root %d",
				 length_count, comm_count, type_count, length,
				 test_nump, comm_type, test_type, root);
			      MPITEST_message(MPITEST_INFO1, info_buf);
			    }
			    /*-------------------------------------------------
			                                   Send
			               ALL nodes send a message to root
			    -------------------------------------------------*/

			    if ((!inter_comm) ||
				(inter_comm && MPITEST_inter == send_group))
			    {
			      loop_cnt++;
			      loop_fail = 0;
					

			      /* Reattach the send buffer for this root,
			         and detach it before switching to new root
			      */

			      berr = MPI_Buffer_attach(buff, buffsize);
			      if (berr != MPI_SUCCESS)
			      {
			          sprintf(info_buf, "Non-zero return code (%d) from MPI_Buffer_attach", berr);
			          MPITEST_message(MPITEST_FATAL, info_buf);
			      }

			      /*
			       * Set up the dataTemplate for initializing
			       * send buffer
			       */

			      MPITEST_dataTemplate_init(&value,
					MPITEST_current_rank);

			      /* Initialize send buffer */
			      MPITEST_init_buffer(test_type, length + 1,
					          value, send_buffer);

			      ierr = MPI_Ibsend(send_buffer, length,
					     MPITEST_mpi_datatypes[test_type],
					     root, root, comm, &send_request );

			      if (ierr != MPI_SUCCESS)
			      {
				  sprintf(info_buf, "Non-zero return code (%d) from MPI_Ibsend", ierr);
				   MPITEST_message(MPITEST_NONFATAL, info_buf);
				   MPI_Error_string(ierr, error_string, &size);
				   MPITEST_message(MPITEST_NONFATAL, error_string);
				   loop_fail++;
			      }	/* Ibsend Error Test  */


                              ierr = MPI_Wait(&send_request, &send_stat);


			      if (ierr != MPI_SUCCESS)
			      {
				  sprintf(info_buf, "Non-zero return code (%d) from MPI_Wait on Ibsend", ierr);
				   MPITEST_message(MPITEST_NONFATAL, info_buf);
				   MPI_Error_string(ierr, error_string, &size);
				   MPITEST_message(MPITEST_NONFATAL, error_string);
				   loop_fail++;
				}  /* End of Wait Error Test  */

			      /* Corrupt Send Buffer  */
			      ((char *) send_buffer)[1] = '?';

			      if (loop_fail != 0) fail++;
			    }	/* End of test for not a sender node */

			    /*-------------------------------------------------
					       POST  RECEIVE
			    The root node posts receives for a message from
                            each of the  nodes
			    -------------------------------------------------*/
			    if ((!inter_comm && MPITEST_current_rank == root)
				|| (inter_comm && MPITEST_inter == recv_group
			             && MPITEST_current_rank == root))
			    {

			      get_from = test_nump;
			      if (inter_comm)
			          get_from = senders;
			      for (i = 0; i < get_from; i++)
			      {
				  loop_cnt++;
				  loop_fail = 0;
				  /* Initialize the receive buffer to -1 */

				  MPITEST_dataTemplate_init(&value, -1);
				  MPITEST_init_buffer(test_type, length + 1,
						value, recv_buffer);

				  ierr = MPI_Irecv(recv_buffer, length+1,
					     MPITEST_mpi_datatypes[test_type],
						    MPI_ANY_SOURCE, root,
						    comm, &recv_request);

				  if (ierr != MPI_SUCCESS)
				  {
					sprintf(info_buf, "Non-zero return code (%d) from MPI_Irecv", ierr);
					MPITEST_message(MPITEST_NONFATAL,
							info_buf);
					MPI_Error_string(ierr, &info_buf[0],
							&size);
					MPITEST_message(MPITEST_NONFATAL,
							info_buf);
					loop_fail++;
				  }	/* Error Test  */


                                  ierr = MPI_Wait(&recv_request, &recv_stat);


			          if (ierr != MPI_SUCCESS)
				  {
				        sprintf(info_buf, "Non-zero return code (%d) from MPI_Wait on Irecv", ierr);
				        MPITEST_message(MPITEST_NONFATAL, info_buf);
				        MPI_Error_string(ierr, error_string, &size);
				        MPITEST_message(MPITEST_NONFATAL, error_string);
				        loop_fail++;
				}	/* End of Wait on Irecv Error Test  */


				  /*
				   * Set up the dataTemplate for checking the
				   * recv'd buffer.  Note that the sending
				   * node rank will be sent.
				   */
				  MPITEST_dataTemplate_init(&value,
					recv_stat.MPI_SOURCE);


				  error = MPITEST_buffer_errors(test_type,
						  length, value, recv_buffer);

				  /* check for receive buffer overflow */
				  MPITEST_dataTemplate_init(&value, -1);

				  error += MPITEST_buffer_errors_ov(test_type,
						  length, value, recv_buffer);

				  if (error)
				  {
				      loop_fail++;
				      sprintf(info_buf, "%d errors in buffer (%d,%d,%d) len %d commsize %d commtype %d data_type %d root %d",
						error, length_count, comm_count,						type_count, length, test_nump,
						comm_type, test_type, root);
				      MPITEST_message(MPITEST_NONFATAL,
						info_buf);
				    }
				    else
				    {
					sprintf(info_buf, "%d errors found in buffer", error);
					MPITEST_message(MPITEST_INFO2, info_buf);
				    }

				    /*
				     * Call the MPI_Get_Count function, and
				     * compare value with length
				     */
				    cnt_len = -1;

				    ierr = MPI_Get_count(&recv_stat,
					    MPITEST_mpi_datatypes[test_type],
					    &cnt_len);
				    if (ierr != MPI_SUCCESS)
				    {
					sprintf(info_buf, "Non-zero return code (%d) from MPI_Get_count", ierr);
					MPITEST_message(MPITEST_NONFATAL,
					    info_buf);
					MPI_Error_string(ierr, &info_buf[0],
					    &size);
					MPITEST_message(MPITEST_NONFATAL,
					    info_buf);
					loop_fail++;
				    }

				    /*
				     * Print non-fatal error if Received lenth
				     * not equal to send length
				     */
				    error = length - cnt_len;

				    if (error)
				    {
				    sprintf(info_buf, "Send/Receive lengths differ - Sender(node/length)=%d/%d,  Receiver(node/length)=%d/%d",
					recv_stat.MPI_SOURCE, length,
					MPITEST_current_rank, cnt_len);
					MPITEST_message(MPITEST_NONFATAL,
							info_buf);
					loop_fail++;
				    }

				    /*
				     * Print non-fatal error if tag is not
				     * correct.
				     */
				    if (recv_stat.MPI_TAG != root)
				    {
				        sprintf(info_buf, "Unexpected tag value=%d, expected=%d",
					    recv_stat.MPI_TAG, root);
					MPITEST_message(MPITEST_NONFATAL, info_buf);
					loop_fail++;
				    }

				if (loop_fail != 0) fail++;

			      }	/* End of for loop:  Current ROOT
				 * receives from all nodes */


			    }  /* End of receive code for current root  */

			  if ((!inter_comm) ||
				(inter_comm && MPITEST_inter == send_group))
			  {
			     /*  Detach Ibsend buffer  */
			      berr = MPI_Buffer_detach(&buff, &bsize);

			      if (berr != MPI_SUCCESS)
			      {
			          sprintf(info_buf, "Non-zero return code (%d) from MPI_Buffer_detach", berr);
			          MPITEST_message(MPITEST_NONFATAL, info_buf);
			          MPI_Error_string(berr, error_string, &size);
			          MPITEST_message(MPITEST_NONFATAL, error_string);
			          fail++;
			      }	/* Buffer detach error Test  */
			  }


			    ierr = MPI_Barrier(barrier_comm);
			    if (ierr != MPI_SUCCESS)
			    {
				sprintf(info_buf, "Non-zero return code (%d) from MPI_Barrier", ierr);
				MPITEST_message(MPITEST_NONFATAL, info_buf);
				MPI_Error_string(ierr, error_string, &size);
				MPITEST_message(MPITEST_NONFATAL, error_string);
				fail++;
			    }
			} /* Loop over Roots  */

		    }	/* End of extra loop for INTER-communicator  */


		}	/* Loop over Message Lengths  */

		free(send_buffer);
		free(recv_buffer);

	    }	/* Loop over Data Types  */

	    if (inter_comm)
	    {
		ierr = MPI_Comm_free(&barrier_comm);
		if (ierr != MPI_SUCCESS)
		{
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_free", ierr);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_FATAL, error_string);
		}
	    }

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

    /* Detach the buffer, and free the dynamic memory */

    berr = MPI_Buffer_detach(&buff, &bsize);

    free(buff);

    /* report overall results  */

    MPITEST_report(loop_cnt - fail, fail, 0, testname);

    MPI_Finalize();

    return fail;

} /* main() */
