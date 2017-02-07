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
                          Test for MPI_Bcast()

The MPI_Bcast function broadcasts a message from one node to all other
nodes in a communicator.  The sending node is referred to as the
'root' node.

This function initializes the send buffer with the root's rank in the
communicator (or an appropriate value for the non-integer types.)
Once the receiving nodes have completed the broadcast, they check to
make sure the current root's rank is in the received buffer.

This test may be run in any communicator, with any data type, and with
any non-negative mwwage length.

The MPITEST environment provides looping over communicator size and
type, message length, and data type.  The properties of the loops are
encoded in configuration arrays in the file mpitest_cfg.h .

******************************************************************************/

#include "mpitest_cfg.h"
#include "mpitest.h"


int main(int argc, char *argv[])
{
   int
      test_type,		/* the index of the current buffer type              */
      length,			/* The length of the current buffer                  */
      byte_length,		/* The length of the current buffer in bytes         */
      test_nump,		/* The number of processors in current
				 * communicator  */
      comm_index,		/* the array index of the current comm               */
      comm_type,		/* the index of the current communicator type        */
      type_count,		/* loop counter for data type loop                   */
      length_count,		/* loop counter for message length loop              */
      comm_count,		/* loop counter for communicator loop                */
      error,			/* errors from one MPI call                          */
      fail,			/* counts total number of failures                   */
      size,			/* return size from MPI_Error_string                 */
      loop_cnt,			/* counts total number of loops through test         */
      ierr,			/* return value from MPI calls                       */
      max_length,		/* maximum buffer length specified in config.
				 * file   */
      max_byte_length,		/* maximum buffer length in bytes                    */
      root,			/* the root of the current broadcast                 */
      i, j;

   struct dataTemplate
      value;			/* dataTemplate for initializing buffers             */
   struct dataTemplate
     *values;			/* Array of dataTemplates for verbose init           */

   void *send_buffer;		/* message buffer                                     */

   char
      info_buf[256],		/* buffer for passing mesages to MPITEST             */
      testname[128];		/* the name of this test                             */
    char error_string[MPI_MAX_ERROR_STRING];

   MPI_Comm comm;		/* MPI communicator                                  */

   int inter_flag;

   ierr = MPI_Init(&argc, &argv);
   if (ierr != MPI_SUCCESS) {
      sprintf(info_buf, "MPI_Init() returned %d", ierr);
      MPITEST_message(MPITEST_FATAL, info_buf);
   }


   sprintf(testname, "MPI_Bcast()");

   MPITEST_init(argc, argv);

   if (MPITEST_me == 0) {
      sprintf(info_buf, "Starting %s test", testname);
      MPITEST_message(MPITEST_INFO0, info_buf);
   }

   /* set the global error counter */
   fail = 0;
   loop_cnt = 0;

   /* find the maximum sized buffer we will use */
   max_byte_length = MPITEST_get_max_message_length();

   for (comm_count = 0; comm_count < MPITEST_num_comm_sizes(); comm_count++) {
      comm_index = MPITEST_get_comm_index(comm_count);
      comm_type = MPITEST_get_comm_type(comm_count);

      test_nump = MPITEST_get_communicator(comm_type, comm_index, &comm);

      if (comm != MPI_COMM_NULL) {
	 ierr = MPI_Comm_test_inter(comm, &inter_flag);
	 if (ierr != MPI_SUCCESS) {
	    fail++;
	    sprintf(info_buf, "MPI_Comm_test_inter() returned %d", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_FATAL, error_string);
	 }
	 
	 if (inter_flag) {
	    /* Ignore inter-communicator for collective functional tests */
	    MPITEST_free_communicator(comm_type, &comm);
	    sprintf(info_buf, "Skipping intercommunicator (commtype: %d) for this test", comm_type);
	    MPITEST_message(MPITEST_INFO1, info_buf);
	    
	    continue;
	 }
	 for (type_count = 0; type_count < MPITEST_num_datatypes(); type_count++) {
	    test_type = MPITEST_get_datatype(type_count);
	    
	    
	    /* convert the number of bytes in the maximum length message */
	    /* into the number of elements of the current type */
	    max_length = MPITEST_byte_to_element(test_type, max_byte_length);
	    
	    /* then allocate the buffer */
	    MPITEST_get_buffer(test_type, max_length, &send_buffer);
	    
	    for (length_count = 0; length_count < MPITEST_num_message_lengths(); length_count++) {
	       byte_length = MPITEST_get_message_length(length_count);
	       length = MPITEST_byte_to_element(test_type, byte_length);
	       
	       for (root = 0; root < test_nump; root++) {
		  /* print an informational message */
		  if (MPITEST_current_rank == 0) {
		     sprintf(info_buf, "(%d,%d,%d) length %d commsize %d commtype %d data_type %d root %d",
			     length_count, comm_count, type_count, length,
			     test_nump, comm_type, test_type, root);
		     MPITEST_message(MPITEST_INFO1, info_buf);
		  }
		  
		  /* Set up the dataTemplate for initializing send buffer */
		  MPITEST_dataTemplate_init(&value, MPITEST_current_rank);

		  /* Initialize send buffer */
		  MPITEST_init_buffer(test_type, length + 1,
				      value, send_buffer);


		  /*
		   * Set up the dataTemplate for checking the received
		   * buffer.  Note that the root's rank will be sent.
		   */
		  MPITEST_dataTemplate_init(&value, root);


		  loop_cnt++;
		  ierr = MPI_Bcast(send_buffer, length,
				   MPITEST_mpi_datatypes[test_type],
				   root, comm);

		  if (ierr != MPI_SUCCESS) {
		     sprintf(info_buf, "MPI_Bcast() returned %d", ierr);
		     MPITEST_message(MPITEST_NONFATAL, info_buf);
		     MPI_Error_string(ierr, error_string, &size);
		     MPITEST_message(MPITEST_FATAL, error_string);
		     fail++;
		  }
		  error = MPITEST_buffer_errors(test_type, length, value, send_buffer);

		  /* check for receive buffer overflow */
		  MPITEST_dataTemplate_init(&value, MPITEST_current_rank);
		  error += MPITEST_buffer_errors_ov(test_type,
						length, value, send_buffer);

		  if (error) {
		     if (ierr == MPI_SUCCESS)
			fail++;
		     sprintf(info_buf, "%d errors in buffer (%d,%d,%d) len %d commsize %d commtype %d data_type %d root %d",
			     error, length_count, comm_count, type_count,
			     length, test_nump, comm_type, test_type, root);
		     MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
		  else {
		     sprintf(info_buf, "%d errors found in buffer", error);
		     MPITEST_message(MPITEST_INFO2, info_buf);
		  }
	       }
	    }
	    free(send_buffer);
      	 }
      }

      MPITEST_free_communicator(comm_type, &comm);
   }
   /* report overall results  */
   MPITEST_report(loop_cnt - fail, fail, 0, testname);

   ierr = MPI_Finalize();
   return 0;
}				/* main() */
