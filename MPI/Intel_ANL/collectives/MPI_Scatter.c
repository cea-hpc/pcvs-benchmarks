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

                           Test for MPI_Scatter()

The MPI_Scatter() function takes a vector of data on one process
(which we refer to as the 'root' process) and sends a small piece of
the vector to each of the other nodes.  If the root's vector is N
bytes long, and there are n processes participating, then each process
should end up with N/n bytes of data.

This test initializes the send buffer with the root's rank.  Then the
Scatter is performed.  All nodes then test their received data to make
sure it contains the root's rank.  For a given communicator, the test
loops over the identity of the root, so that each process in the
communicator acts once as root.

The MPITEST environment provides looping over message length, data
type, and communicator size and type, as specified in the file
mpitest_cfg.h .

Revision History:
  1               gm   Original version.
  2     2/1/96    gt   Reduced size of send buffer to stay within bounds,
                       and modified data sent/expected.
******************************************************************************/

#include "mpitest_cfg.h"
#include "mpitest.h"

int main(int argc, char *argv[])
{
   int
      test_type,		/* the index of the current buffer type              */
      length,			/* The length of the current buffer                  */
      byte_length,		/* length of the current buffer in bytes             */
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
      max_byte_length,		/* max buffer length in bytes                        */
      root,			/* the current gather root                            */
      i, j;			/* index variables                                    */

   struct dataTemplate
      value;			/* dataTemplate for initializing buffers             */
   struct dataTemplate
     *values;			/* Array of dataTemplates for verbose init           */

   void *recv_buffer,		/* buffers for sending and receiving                 */
     *send_buffer;

   int				/* for verbose initialization or checking            */
     *counts,			/* the array of item counts                       */
     *displs;			/* the array of displacements from buffer[0]      */

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

   sprintf(testname, "MPI_Scatter()");

   MPITEST_init(argc, argv);

   if (MPITEST_me == 0) {
      sprintf(info_buf, "Starting %s test", testname);
      MPITEST_message(MPITEST_INFO0, info_buf);
   }

   max_byte_length = MPITEST_get_max_message_length();

   /* set the global error counter */
   fail = 0;
   loop_cnt = 0;

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
	 /* buffers for error checking */
	 counts = (int *) malloc(test_nump * sizeof(int));
	 displs = (int *) malloc(test_nump * sizeof(int));
	 values = (struct dataTemplate *) malloc(test_nump * sizeof(struct dataTemplate));
	 
	 if (!counts || !displs || !values)
	    MPITEST_message(MPITEST_FATAL, "Couldn't allocate test arrays");
	 
	 for (type_count = 0; type_count < MPITEST_num_datatypes(); type_count++) {
	    test_type = MPITEST_get_datatype(type_count);
	    
	    max_length = MPITEST_byte_to_element(test_type, max_byte_length);
	    
	    if (max_length < MPITEST_nump)
	       max_length = MPITEST_nump;	/* rev2 */
	    max_length = max_length / MPITEST_nump;	/* rev2 */
	    
	    MPITEST_get_buffer(test_type, max_length, &recv_buffer);
	    MPITEST_get_buffer(test_type, max_length * MPITEST_nump, &send_buffer);
	    
	    for (length_count = 0; length_count < MPITEST_num_message_lengths(); length_count++) {
	       byte_length = MPITEST_get_message_length(length_count);
	       length = MPITEST_byte_to_element(test_type, byte_length);
	       
	       if (length > max_length)
		  length = max_length;	/* rev2 */
	       
	       for (root = 0; root < test_nump; root++) {
		  if (MPITEST_current_rank == 0) {
		     sprintf(info_buf, "(%d,%d,%d) length %d commsize %d commtype %d data_type %d gather root %d",
			     length_count, comm_count, type_count, length,
			     test_nump, comm_type, test_type, root);
		     MPITEST_message(MPITEST_INFO1, info_buf);
		  }
		  
		  /* Set up dataTemplate for initializing send buffer */
		  MPITEST_dataTemplate_init(&value, MPITEST_current_rank);

		  /* Initialize send buffer */
		  MPITEST_init_buffer_inc(test_type, test_nump * length,
					  value, send_buffer);	/* rev2 */

		  /* Set up dataTemplate for initializing recv buffer */
		  MPITEST_dataTemplate_init(&value, -1);

		  /* Initialize recv buffer */
		  MPITEST_init_buffer(test_type, length + 1,
				      value, recv_buffer);

		  /* Set up dataTemplate for error checking recv buffer */
		  MPITEST_dataTemplate_init(&value,
				      root + length * MPITEST_current_rank);	/* rev2 */

		  loop_cnt++;
		  ierr = MPI_Scatter(send_buffer, length,
				     MPITEST_mpi_datatypes[test_type],
				     recv_buffer, length,
				     MPITEST_mpi_datatypes[test_type],
				     root, comm);

		  if (ierr != MPI_SUCCESS) {
		     sprintf(info_buf, "MPI_Scatter() returned %d", ierr);
		     MPITEST_message(MPITEST_NONFATAL, info_buf);
		     MPI_Error_string(ierr, error_string, &size);
		     MPITEST_message(MPITEST_FATAL, error_string);
		     fail++;
		  }

		  error = 0;
		  error = MPITEST_buffer_errors_inc(test_type, length, value,
						    recv_buffer);

		  /* check for recv_buffer overflow */
		  MPITEST_dataTemplate_init(&value, -1);
		  error += MPITEST_buffer_errors_ov(test_type,
						length, value, recv_buffer);

		  if (error) {
		     if (ierr == MPI_SUCCESS)
			fail++;
		     sprintf(info_buf, "%d errors in buffer (%d,%d,%d) len %d commsize %d commtype %d data_type %d root %d",
			error, length_count, comm_count, type_count, length,
			     test_nump, comm_type, test_type, root);
		     MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
		  else {
		     sprintf(info_buf, "%d errors found in buffer", error);
		     MPITEST_message(MPITEST_INFO2, info_buf);
		  }

	       }		/***** if (MPITEST_current_rank != MPI_UNDEFINED) *****/
	    }			/***** for (root=0; ....) ********/

	    free(recv_buffer);
	    free(send_buffer);

	 }			/****** for (length_count=0;...) *********/

	 free(counts);
	 free(displs);
	 free(values);

      }				/****** for (type_count=0;....) *********/

      MPITEST_free_communicator(comm_type, &comm);
   }				/****** for (comm_count=0;...) ***********/
   /* report overall results  */
   MPITEST_report(loop_cnt - fail, fail, 0, testname);

   MPI_Finalize();
   return 0;
}				/* main() */
