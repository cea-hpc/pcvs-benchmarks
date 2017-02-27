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
      Test for MPI_Isend(): ROOT send to ALL with BYTE OFFSETS
      Receive with MPI_Irecv()


This is a modified version of the  MPI_Isend  INTER-COMMUNICATOR version
of the ISEND FROM ROOT TO ALL. The program uses only the MPI_CHAR data type.
The charater records are then offset by one to three bytes for both send and
receive as a test for non-word address boundary conditions.

The program selects each node, in turn, to be the root.  The ROOT ISENDS to
each of the other nodes.  In this test ROOT does NOT send to itself.

This test initializes the send buffer with the root's rank in the
communicator (or an appropriate value for the non-integer types.)
Once the receiving nodes have completed their message receive, they check to
make sure the current root's rank is in the received buffer.

This test may be run in any communicator, with any non-negative message length.

The MPITEST environment provides looping over message lengths.  The properties
of the loops are encoded in configuration arrays in the file mpitest_cfg.h.  See the
MPITEST README for further details.
******************************************************************************/

#include "mpitest_cfg.h"
#include "mpitest.h"


int main( int argc, char *argv[])
{
int
test_type,         /*  the index of the current buffer type              */
length,            /*  The length of the current buffer                  */
byte_length,       /*  The length of the current buffer in bytes         */
cnt_len,           /*  received length returned by MPI_Get_Count         */
type_count,        /*  loop counter for data type loop                   */
length_count,      /*  loop counter for message length loop              */
error,             /*  errors from one MPI call                          */
fail,              /*  counts total number of failures                   */
loop_fail,         /*  counts number of failures in a loop               */
size,              /*  return size from MPI_Error_string                 */
loop_cnt,          /*  counts total number of loops through test         */
ierr,              /*  return value from MPI calls                       */
max_length,        /*  maximum buffer length specified in config. file   */
max_byte_length,   /*  maximum buffer length in bytes                    */
root,              /*  the root of the current broadcast                 */
print_node_flag,   /*  node 0 of INTRA, or node 0 of left group of INTER */
send_to,           /*  Number of nodes that will receive current message */
offset,            /*  index in for loop to vary offset                  */
send_off,          /*  byte offset for character send buffer             */
recv_off,          /*  byte offset for character receive buffer          */
i, j;              /*  utility loop index variables                      */

struct dataTemplate
value;             /*  dataTemplate for initializing buffers             */
struct dataTemplate
*values;           /*  Array of dataTemplates for verbose init           */

char  *recv_buffer;
char  *send_buffer;  /* message buffer                                   */

char
info_buf[256],     /*  buffer for passing mesages to MPITEST             */
testname[64];      /*  the name of this test                             */
    char error_string[MPI_MAX_ERROR_STRING];

MPI_Comm
comm;              /*  MPI communicator                                  */

MPI_Status
recv_stat,         /*  MPI  status structure                             */
send_stat,	
stat;

MPI_Request
recv_request,      /*  MPI request structure                             */
send_request;      /*  MPI request structure                             */
 

/*------------------------------  MPI_Init  ---------------------------------*/
  cnt_len = -1;

  ierr = MPI_Init(&argc, &argv);
  if( ierr!=MPI_SUCCESS)
   {
    sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr);
    MPITEST_message(MPITEST_FATAL, info_buf);
   }

  sprintf( testname, "MPI_Isend_off: character OFFset");

/*-----------------------------  MPITEST_init  ------------------------------*/
  MPITEST_init( argc, argv);
  if(MPITEST_me==0)
   {
    sprintf(info_buf, "Starting %s test", testname);
    MPITEST_message( MPITEST_INFO0, info_buf);
   }

  /* set the global error counter */
  fail = 0;
  loop_cnt = 0;

  max_byte_length = MPITEST_get_max_message_length();
  comm = MPI_COMM_WORLD;
  print_node_flag = 1;

/*--------------------------  Loop over Data Types  -------------------------*/

/*  For this one we want to do just test type character, but
    we want to run the character send with charater offsets, so we'll force
    it here.
*/
  MPITEST_types[0] = MPITEST_char;
  type_count = 0;
  test_type = MPITEST_get_datatype(type_count);

  /* convert the number of bytes in the maximum length message */
  /*  into the number of elements of the current type */

  max_length = MPITEST_byte_to_element( test_type, max_byte_length);

  /*  Allocate send and receive Buffers */
  MPITEST_get_buffer( test_type, max_length+4, (void *)&recv_buffer);
  MPITEST_get_buffer( test_type, max_length+4, (void *)&send_buffer);

  for(length_count=0;length_count<MPITEST_num_message_lengths();length_count++)
   {
    byte_length = MPITEST_get_message_length( length_count);
    length = MPITEST_byte_to_element( test_type, byte_length);
/*-------------------------  Loop over offsets  -----------------------------*/

    for(offset=0; offset<4; offset++)
      {
        send_off = offset;
        recv_off = 3 - offset;

/*-------------------------  Loop over roots  -------------------------------*/
/*
    We loop over the roots, with each root sending
    to every other node in the communicator.
*/
        for( root=0; root<MPITEST_nump; root++)
	 {
	  if (print_node_flag)
	   {
	    sprintf(info_buf, "length %d commsize %d data_type %d root %d",
		      length, MPITEST_nump, test_type, root);
	    MPITEST_message( MPITEST_INFO1, info_buf);

	   }
/*----------------------------- -----------------------------------------------
		   POST  RECEIVES
Each node of send group gets to be root and send to all nodes in receive group
-----------------------------------------------------------------------------*/
	  if( MPITEST_me != root)
	   {
	    /*  Initialize the receive buffer to -1 */
	    MPITEST_dataTemplate_init( &value, -1);
	    MPITEST_init_buffer( test_type, length+1,
	    	      value, recv_buffer+recv_off);
	    loop_cnt++;
	    loop_fail = 0;

	    ierr = MPI_Irecv(recv_buffer+recv_off,
			    length+1,
			     MPITEST_mpi_datatypes[test_type],
			     root, 
			     root, 
			     comm, 
			     &recv_request);

	    if (ierr!=MPI_SUCCESS)
	     {
	      sprintf( info_buf, "Non-zero return code (%d) from MPI_Irecv", ierr);
	      MPITEST_message( MPITEST_NONFATAL, info_buf);
	      MPI_Error_string(ierr, error_string, &size);
	      MPITEST_message( MPITEST_NONFATAL, info_buf);
	      loop_fail++;
	     } /*  Error Test  */

            ierr = MPI_Wait(&recv_request, &recv_stat);

	    if (ierr != MPI_SUCCESS)
	     {
	      sprintf(info_buf, "Non-zero return code (%d) from MPI_Wait on Irecv", ierr);
	      MPITEST_message(MPITEST_NONFATAL, info_buf);
	      MPI_Error_string(ierr, error_string, &size);
	      MPITEST_message(MPITEST_NONFATAL, error_string);
	      loop_fail++;
	     }	/* End of Wait Error Test  */


	    /*  Set up the dataTemplate for checking the recv'd buffer.
	        Note that the sending node rank will be sent. */

	    MPITEST_dataTemplate_init( &value, recv_stat.MPI_SOURCE );
	    error = MPITEST_buffer_errors_inc( test_type,
			      length, value, recv_buffer+recv_off);

	    /* check for receive buffer overflow */
	    MPITEST_dataTemplate_init( &value, -1);
	    error += MPITEST_buffer_errors_ov( test_type,
		      length, value, recv_buffer+recv_off);

	    if (error)
	     {
	      sprintf(info_buf, "%d errors in buffer len %d commsize %d data_type %d root %d",
		      error, length, MPITEST_nump, test_type, root);
	      loop_fail++;
	      MPITEST_message(MPITEST_NONFATAL, info_buf);
	     } else {
	      sprintf(info_buf,"%d errors found in buffer", error);
	      MPITEST_message(MPITEST_INFO2, info_buf);
	     }


	    /*  Call the MPI_Get_Count function, and compare value with
	         length  */
	    cnt_len = -1;

	    ierr = MPI_Get_count( &recv_stat, MPITEST_mpi_datatypes[test_type], &cnt_len );
	    if( ierr!=MPI_SUCCESS)
	     {
	      sprintf( info_buf, "Non-zero return code (%d) from MPI_Get_count", ierr);
	      MPITEST_message( MPITEST_NONFATAL, info_buf);
	      MPI_Error_string(ierr, error_string, &size);
	      MPITEST_message( MPITEST_NONFATAL, info_buf);
	      loop_fail++;
	     }


	    /*  Print non-fatal error if Received length not equal to send
	        length  */
	    error = length - cnt_len;

	    if(error)
	     {
	      sprintf(info_buf, "Send/Receive lengths differ - Sender(node/length)=%d/%d,  Receiver(node/length)=%d/%d",
	          recv_stat.MPI_SOURCE, length,
	          MPITEST_me, cnt_len);
	      MPITEST_message(MPITEST_NONFATAL, info_buf);
	      loop_fail++;
	     }

	    if (loop_fail != 0) fail++;
	   } /* End of receive for loop */


/*-----------------------------------------------------------------------------
			       Isend
     Root node sends to all other nodes
-----------------------------------------------------------------------------*/

/*  Send from each root node to all other nodes            */

	  if (MPITEST_me==root)
	   {
	    /*  Set up the dataTemplate for initializing send buffer */
	    MPITEST_dataTemplate_init( &value, -1);
	    MPITEST_init_buffer_inc( test_type, 4,
		      value, send_buffer);

	    MPITEST_dataTemplate_init( &value, MPITEST_me);
	    MPITEST_init_buffer_inc( test_type, length+1,
		      value, send_buffer+send_off);

	    /*  Since root doesn't receive a message from itself, it should
	        receive messages from MPITEST_nump-1 other nodes.
	    */

	    for  (i=0; i<MPITEST_nump; i++ )
             {
	      if (i == root)
		continue;

	      loop_cnt++;
	      loop_fail = 0;

	      ierr = MPI_Isend(send_buffer+send_off,
			      length,
			      MPITEST_mpi_datatypes[ test_type],
			      i ,
			      root, 
			      comm,
			      &send_request);

	      if (ierr != MPI_SUCCESS)
	       {
		 sprintf( info_buf, "Non-zero return code (%d) from MPI_Isend", ierr);
		 MPITEST_message( MPITEST_NONFATAL, info_buf);
		 MPI_Error_string(ierr, error_string, &size);
		 MPITEST_message( MPITEST_NONFATAL, info_buf);
		 loop_fail++;
		} /*  Error Test  */

              ierr = MPI_Wait(&send_request, &send_stat);

	      if (ierr != MPI_SUCCESS)
	       {
		 sprintf(info_buf, "Non-zero return code (%d) from MPI_Wait on Isend", ierr);
		 MPITEST_message(MPITEST_NONFATAL, info_buf);
       		 MPI_Error_string(ierr, error_string, &size);
		 MPITEST_message(MPITEST_NONFATAL, error_string);
		 loop_fail++;
	       }
	
	      if (loop_fail != 0) fail++;
             } /* End of for loop: Send from current root to all others  */

           } /* if I am sender */

   	  MPI_Barrier(comm);
	 }  /*  Loop over Roots  */

       } /* End of loop for offsets  */

   }  /*  Loop over Message Lengths  */

  free( send_buffer);
  free( recv_buffer);


/*---------------------------------------------------------------------------*/


  /* report overall results  */

  MPITEST_report( loop_cnt-fail, fail, 0, testname);

  MPI_Finalize();

  return fail;

} /* main() */

