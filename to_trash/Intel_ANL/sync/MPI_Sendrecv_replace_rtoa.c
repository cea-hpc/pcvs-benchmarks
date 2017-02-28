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

               MPI_Sendrecv_replace funtional Root to All test

Ref:  MPI Standard, Section 3.10
	
This test may be run in any communicator, with any data type, and with
any non-negative message length.

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
byte_length,       /*  The length of the current buffer in bytes         */
node_cnt_len,      /*  node received length returned by MPI_Get_Count    */
root_cnt_len,      /*  root received length returned by MPI_Get_Count    */
comm_count,        /*  loop counter for communicator loop                */
comm_index,        /*  the array index of the current comm               */
comm_type,         /*  the index of the current communicator type        */
error,             /*  errors from one MPI call                          */
fail,              /*  counts total number of failures                   */
loop_fail,         /*  counts number of failures in loop                 */
grp_lup,           /*  index for comm groups (1 to ntimes)               */
i, j,              /*  utility loop index variables                      */
ierr,              /*  return value from MPI calls                       */
inter_comm,        /*  Intercommunicator flag, true, if intercommunicator*/
left_recv_size,    /*  INTER: absolute count of nodes doing left receive */
left_send_size,    /*  INTER absolute(same for all nodes) left send size */

node_all_length,   /*  The length of the current buffer                  */
root_all_length,
length_count,      /*  loop counter for message length loop              */
loop_cnt,          /*  counts total number of loops through test         */
max_byte_length,   /*  maximum buffer length in bytes                    */
max_length,        /*  maximum buffer length specified in config. file   */
ntimes,            /*  # of communicator groups INTRA=1, INTER=2         */
print_node_flag,   /*  node 0 of INTRA, or node 0 of left group of INTER */
receivers,         /*  INTER-comm #receivers for this send/rec choice    */
recv_group,        /*  INTER-comm test receiving group(opposite of send#)*/
recv_size,         /*  INTER relative receive size MPI_Comm_remote_size  */
right_recv_size,   /*  INTER: absolute count of right receiving nodes    */
right_send_size,   /*  INTER: absolute count of right sending nodes      */
root,              /*  the root of the current broadcast                 */
send_group,        /*  INTER-comm test sending group #(0 or 1)           */
send_size,         /*  INTER: node relative send size from MPI_Comm_size */
send_to,           /*  Number of nodes that will receive current message */
senders,           /*  INTER-comm #senders   for this send/rec choice    */
size,              /*  return size from MPI_Error_string                 */
test_nump,         /*  The number of processors in current communicator  */
test_type,         /*  the index of the current buffer type              */
type_count,        /*  loop counter for data type loop                   */
*ub,               /*  Pointer to value of maximum tag                   */
found;		   /* boolean for MPI_Get_attr()			 */

struct dataTemplate
node_value,             /*  dataTemplate for initializing buffers        */
root_value;

void  *node_all_buffer;  /* message buffer                              */
void  *root_all_buffer;

char
info_buf[256],     /*  buffer for passing mesages to MPITEST             */
testname[64];      /*  the name of this test                             */
    char error_string[MPI_MAX_ERROR_STRING];

MPI_Comm
comm,              /*  MPI communicator                                  */
barrier_comm;      /* If intercommunicator, merged comm for Barrier      */

MPI_Status
node_stat,
root_stat;

    /*------------------------------  MPI_Init  -----------------------------*/
    ierr = MPI_Init(&argc, &argv);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr);
	MPITEST_message(MPITEST_FATAL, info_buf);
    }

    sprintf(testname, "MPI_Sendrecv: Root to all model");

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

	/*
	 * This test is valid for Inter-communicators, and tests for a node
	 * that is not part of any communicator
	 */
	if (MPITEST_current_rank != MPI_UNDEFINED)	/* Skip to barrier if
							 * not part of this
							 * communicator */
	{
	    /*
	     * Test if INTER-communicator:  inter_comm is true if this is an
	     * inter-communicator
	     */

	    ierr = MPI_Comm_test_inter(comm, &inter_comm);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_test_inter", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
	    }

	    /*
	     * Variables, send_size, and recv_size  both default to the total
	     * nodes in the communicator for an INTRA-communicator For an
	     * INTER-communicator: MPI_Comm_size returns the size of the
	     * communicator for all of the nodes in each respective communicator
	     * MPI_Comm_remote_size returns the size of the opposite
	     * communicator for all nodes in each respective communicator
	     *
	     * Set the print_node_flag: This is flag tells which rank is
	     * to print.  For INTRA-communition it is simply node 0.  For
	     * INTER-communication it is node zero of the left group.
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
		    {   /* Node zero of left bank of INTER-communicator */
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
		{   /* Do this for right group of an INTER-communicator  */
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
	    /*----------------  Loop over Data Types  ----------------------*/

	    for (type_count = 0; type_count < MPITEST_num_datatypes(); type_count++)
	    {
		test_type = MPITEST_get_datatype(type_count);

		/* convert the number of bytes in the maximum length message */
		/* into the number of elements of the current type */

		max_length = MPITEST_byte_to_element(test_type, max_byte_length);

		/* Allocate send and receive Buffers */

		MPITEST_get_buffer(test_type, max_length, &node_all_buffer);
		MPITEST_get_buffer(test_type, max_length, &root_all_buffer);

		/*-------------  Loop over Message Lengths  -----------------*/

		for (length_count = 0; length_count < MPITEST_num_message_lengths(); length_count++)
		{
		    byte_length = MPITEST_get_message_length(length_count);

		    root_all_length = MPITEST_byte_to_element(test_type, byte_length);
                    node_all_length = root_all_length;

		    /*--------------  Loop over roots  ----------------------*/

		    /*
		     * For an intra-communicator, we loop over all the nodes,
		     * with each node being a root, and sending to all of the
		     * other nodes. For an inter-communicator setting, we have
		     * two sets of nodes, in left and right sub-communicators,
		     * and each node in both sub-communicators gets to be the
		     * root, and send to all the nodes of the other
		     * sub-communicator.
		     */

		    i = -1;
		    j = -1;

		    /*
		     * Extra loop for INTER-communicators to have each group
		     * send and receive
		     */
		    ntimes = 1;
		    if (inter_comm)
			ntimes = 2;

		    /*
		     * For grp_lup = 1 send from left  sub-communicator to
		     * right sub-communicator
		     */

		    /*
		     * For grp_lup = 2 send from right sub-communicator to left
		     * sub-communicator
		     */

		    for (grp_lup = 1; grp_lup <= ntimes; grp_lup++)
		    {
			send_group = grp_lup - 1;
			recv_group = 2 - grp_lup;

			/*
			 * Set up the number of senders and  receivers
			 * depending on sub-group of INTER-communicator
			 */

			senders = test_nump;
			receivers = test_nump - 1;
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
			for (root = 0; root < senders; root++)
			{
			    /* print an informational message */

			    if (print_node_flag)
			    {
				sprintf(info_buf, "(%d,%d,%d) node_all_length %d commsize %d commtype %d data_type %d root %d",length_count, comm_count, type_count, node_all_length, test_nump, comm_type, test_type, root);
				MPITEST_message(MPITEST_INFO1, info_buf);
			    }

			    /*-------------------------------------------------
					       POST  RECEIVES
			    INTRA:  All non-root nodes post a receive for a
				    message from the current root
			    INTER:  Each node of send group gets to be root
				    and send to all nodes in receive group
			    -------------------------------------------------*/

			    if ((!inter_comm && MPITEST_current_rank != root)
			     ||  (inter_comm && MPITEST_inter == recv_group))
			    {

				/* Initialize the node receive buffer to -1 */

				MPITEST_dataTemplate_init(&node_value, -1);
				MPITEST_init_buffer(test_type, node_all_length + 1,node_value, node_all_buffer);

				MPITEST_dataTemplate_init(&node_value, MPITEST_current_rank);
				MPITEST_init_buffer(test_type, node_all_length, node_value, node_all_buffer);
				loop_cnt++;
				loop_fail = 0;
       
                               ierr = MPI_Sendrecv_replace(
                                      node_all_buffer,
				      node_all_length,  
				      MPITEST_mpi_datatypes[ test_type], 
				      root,
				      MPITEST_current_rank, 
 		                      root, 
			              root, 
			              comm, 
			              &node_stat);


				if (ierr != MPI_SUCCESS)
				{
				    sprintf(info_buf, "Non-zero return code (%d) from node %d  MPI_Sendrecv_replace", ierr,  MPITEST_current_rank );
				    MPITEST_message(MPITEST_NONFATAL, info_buf);
				    MPI_Error_string(ierr, error_string, &size);
				    MPITEST_message(MPITEST_NONFATAL, error_string);
				    loop_fail++;
				}	/* End of node Sendrecv_replace Error Test  */
        
				/*
				 * Set up the dataTemplate for checking the
				 * nodes recv'd buffer.
				 */

				MPITEST_dataTemplate_init(&node_value, root);
				error = MPITEST_buffer_errors(test_type, node_all_length, node_value, node_all_buffer);

				/* check for receive buffer overflow */
				MPITEST_dataTemplate_init(&node_value, -1);
				error += MPITEST_buffer_errors_ov(test_type, node_all_length, node_value, node_all_buffer);

				if (error)
				{
				    sprintf(info_buf, "%d errors in buffer (%d,%d,%d) len %d commsize %d commtype %d data_type %d root %d", error, length_count, comm_count, type_count, node_all_length, test_nump, comm_type, test_type, root);
				    MPITEST_message(MPITEST_NONFATAL, info_buf);
				    loop_fail++;
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
				node_cnt_len = -1;

				ierr = MPI_Get_count(&node_stat,
				    MPITEST_mpi_datatypes[test_type], &node_cnt_len);
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

				error = node_all_length - node_cnt_len;

				if (error)
				{
				    sprintf(info_buf, "Node Send/Receive lengths differ - Sender(node/length)=%d/%d,  Receiver(node/length)=%d/%d", node_stat.MPI_SOURCE, node_all_length, MPITEST_current_rank, node_cnt_len);
				    MPITEST_message(MPITEST_NONFATAL, info_buf);
				    loop_fail++;
				}

				/*
				 * Print non-fatal error if tag is not
				 * correct.
				 */
				if (node_stat.MPI_TAG != root)
				{
				    sprintf(info_buf, "Unexpected tag value=%d, expected=%d",
					node_stat.MPI_TAG,  root);
				    MPITEST_message(MPITEST_NONFATAL, info_buf);
				    loop_fail++;
				}

				/*
				 * Print non-fatal error if source is not
				 * correct.
				 */
				if (node_stat.MPI_SOURCE != root)
				{
				    sprintf(info_buf, "Unexpected source value=%d, expected=%d",
					node_stat.MPI_SOURCE, root);
				    MPITEST_message(MPITEST_NONFATAL, info_buf);                                    loop_fail++;
				}

			    	if (loop_fail != 0) fail++;

			    }	/* End of for loop:  Receives from INTRA root
				 * sender or INTER send group root */


			    /*-------------------------------------------------
						  Root Sends
				 INTRA:  Root node sends to all other nodes
				 INTER:  Root node in send group sends to all
					 nodes of receive group
			    -------------------------------------------------*/

			    /* Send from each root node to all other nodes   */

			    if ((!inter_comm && MPITEST_current_rank == root)
			      ||(inter_comm && MPITEST_inter == send_group &&
				MPITEST_current_rank == root))

			    {

				send_to = test_nump;
				if (inter_comm)
				    send_to = receivers;
				for (i = 0; i < send_to; i++)
				{

				    if (!inter_comm && i == root)
					/* INTRA comms don't
					 * send to self       */
					continue;

				/*
				 * Set up the dataTemplate for initializing
				 * send/recv  buffer
				 */

				/* Initialize the send/recv buffer to -1 */

				MPITEST_dataTemplate_init(&root_value, -1);
				MPITEST_init_buffer(test_type, root_all_length + 1, root_value, root_all_buffer);
				MPITEST_dataTemplate_init(&root_value, root);

				MPITEST_init_buffer(test_type, root_all_length, root_value, root_all_buffer);
				    loop_cnt++;
				    loop_fail = 0;

       
                               ierr = MPI_Sendrecv_replace(
                                      root_all_buffer,
				      root_all_length,  
				      MPITEST_mpi_datatypes[ test_type], 
				      i,
				      root, 
 		                      i, 
			              i, 
			              comm, 
			              &root_stat);


        			    if (ierr != MPI_SUCCESS)
				    {
	       			        sprintf(info_buf, "Non-zero return code (%d) from root MPI_Sendrecv_replace", ierr);
					MPITEST_message(MPITEST_NONFATAL,
						info_buf);
					MPI_Error_string(ierr, &info_buf[0],
						&size);
					MPITEST_message(MPITEST_NONFATAL,
						info_buf);
					loop_fail++;
				    }	/* Sendrecv_replace Error Test  */
				

				    /*
				     * Set up the dataTemplate for checking the
				     * recv'd buffer.  Note that the sending
				     * node rank will be sent.
				     */
				    MPITEST_dataTemplate_init(&root_value, root_stat.MPI_SOURCE);


				    error = MPITEST_buffer_errors(test_type, root_all_length, root_value, root_all_buffer);

				    /* check for receive buffer overflow */
				    MPITEST_dataTemplate_init(&root_value, -1);

				    error += MPITEST_buffer_errors_ov(test_type, root_all_length, root_value, root_all_buffer);

				    if (error)
				    {
					sprintf(info_buf, "%d errors in buffer (%d,%d,%d) len %d commsize %d commtype %d data_type %d root %d", error, length_count, comm_count, type_count, root_all_length, test_nump, comm_type, test_type, root);
					MPITEST_message(MPITEST_NONFATAL, info_buf);
					loop_fail++;
				    }
				    else
				    {
					sprintf(info_buf, "%d errors found in buffer", error);
					MPITEST_message(MPITEST_INFO2, info_buf);
				    }

				    /*
				     * Call the MPI_Get_Count function, and
				     * compare root_value with length
				     */
				    root_cnt_len = -1;

				    ierr = MPI_Get_count(&root_stat,
					MPITEST_mpi_datatypes[test_type], &root_cnt_len);
				    if (ierr != MPI_SUCCESS)
				    {
					sprintf(info_buf, "Non-zero return code (%d) from Root:  MPI_Get_count", ierr);
					MPITEST_message(MPITEST_NONFATAL, info_buf);
					MPI_Error_string(ierr, error_string, &size);
					MPITEST_message(MPITEST_NONFATAL, error_string);
					loop_fail++;
				    }

				    /*
				     * Print non-fatal error if Received lenth
				     * not equal to send length
				     */

				    error = root_all_length - root_cnt_len;

				    if (error)
				    {
					sprintf(info_buf, "Root: Send/Recv lengths differ - Sender(node/length)=%d/%d,  Receiver(node/length)=%d/%d", root_stat.MPI_SOURCE, root_all_length, root, root_cnt_len);
					MPITEST_message(MPITEST_NONFATAL, info_buf);
					loop_fail++;
				    }


				    /*
				     * Print non-fatal error if tag is not
				     * correct.
				     */
				    if (root_stat.MPI_TAG != root_stat.MPI_SOURCE)
				    {
					sprintf(info_buf, "Unexpected root recvtag root_value=%d, expected=%d",
					root_stat.MPI_TAG, root_stat.MPI_SOURCE);
					MPITEST_message(MPITEST_NONFATAL, info_buf);
					loop_fail++;
				    }



				}	/* End of for loop:  Send from current
					 * root to all other nodes  */

				if (loop_fail != 0) fail++;

			    }	/* End of test for not root node  */


			    ierr = MPI_Barrier(barrier_comm);
			    if (ierr != MPI_SUCCESS)
			    {
			        sprintf(info_buf, "Non-zero return code (%d) from MPI_Barrier", ierr);
			        MPITEST_message(MPITEST_NONFATAL, info_buf);
			        MPI_Error_string(ierr, error_string, &size);
			        MPITEST_message(MPITEST_NONFATAL, error_string);
			        fail++;
			    }

			}	/* Loop over Roots  */


		    }	/* End of extra loop for INTER-communicator  */

		}	/* Loop over Message Lengths  */

		free(root_all_buffer);
		free(node_all_buffer);
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

    }	/* Loop over Communicators  */

    /* report overall results  */

    MPITEST_report(loop_cnt - fail, fail, 0, testname);

    MPI_Finalize();

    return fail;

}/* main() */
