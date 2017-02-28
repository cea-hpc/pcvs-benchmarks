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
                        Test for MPI_Alltoall()

The function MPI_Alltoall() causes each process to send a message to every other
process.  The j_th process' message to the i_th process comes from the i_th
part of j's send buffer, and is received into the j_th part of i's receive
buffer.  All messages are of the same length, regardless of sender or receiver.

This test initializes the buffers in such a way that the receiving process
can know not only the sending process' identity, but also which piece of the
sender's data was received.  This is accomplished by setting the i_th chunk
of data in the j_th node's send buffer to 1000*j+i.  (The i_th chunk of data
on any node will be sent to the i_th node.)  Then, if the receiving
process is process j, then its  i_th chunk of received data (which originated
on the i_th node) should have the value i*1000+j.

This test has looping over communicators, data types, and message lengths as
provided by the MPITEST environment.

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
      i, j;

   struct dataTemplate
      value;			/* dataTemplate for initializing buffers             */
   struct dataTemplate
     *values;			/* Array of dataTemplates for verbose init           */

   int
     *counts,			/* buffers for error checking and verbose            */
     *displs;			/* initialization                                   */

   void
     *send_buffer,		/* message buffer                                     */
     *recv_buffer;

   char
      info_buf[256],		/* buffer for passing mesages to MPITEST             */
      testname[128];		/* teh name of the current test                      */
    char error_string[MPI_MAX_ERROR_STRING];

   MPI_Comm comm;		/* MPI communicator                                  */

   int inter_flag;


   ierr = MPI_Init(&argc, &argv);
   if (ierr != MPI_SUCCESS) {
      sprintf(info_buf, "MPI_Init() returned %d", ierr);
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

   sprintf(testname, "MPI_Alltoall()");

   MPITEST_init(argc, argv);

   if (MPITEST_me == 0) {
      sprintf(info_buf, "Starting %s test", testname);
      MPITEST_message(MPITEST_INFO0, info_buf);
   }

   /* set the global error counter */
   fail = 0;
   loop_cnt = 0;

   /* loop over communicators specified in mpitest_cfg.h/MPITEST_comm_sizes[] */
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
	 /* allocate buffers for verbose init. and error checking */
	 counts = (int *) malloc(test_nump * sizeof(int));
	 displs = (int *) malloc(test_nump * sizeof(int));
	 values = (struct dataTemplate *) malloc(test_nump * sizeof(struct dataTemplate));
	 
	 if (!counts || !displs || !values) {
	    MPITEST_message(MPITEST_FATAL, "Not enough memory for test buffers");
	 }
	 
	 
	 /* loop over data typed specified in mpitest_cfg.h/MPITEST_types[] */
	 for (type_count = 0; type_count < MPITEST_num_datatypes(); type_count++) {
	    test_type = MPITEST_get_datatype(type_count);
	    
	    /* find the maximum sized buffer we will use */
	    max_byte_length = MPITEST_get_max_message_length();
	    
	    /*
	     * convert the number of bytes to the number of elements of the
	     * current type
	     */
	    max_length = MPITEST_byte_to_element(test_type, max_byte_length);
	    if (max_length < MPITEST_nump)
	       max_length = MPITEST_nump;	/* rev2 */
	    max_length = max_length / MPITEST_nump;	/* rev2 */
	    
	    /* then allocate the buffers */
	    MPITEST_get_buffer(test_type, MPITEST_nump * max_length, &send_buffer);
	    MPITEST_get_buffer(test_type, MPITEST_nump * max_length, &recv_buffer);
	    
	    /* loop over message lengths from mpitest_cfg.h/MPITEST_message_lengths[] */
	    for (length_count = 0; length_count < MPITEST_num_message_lengths(); length_count++) {
	       /* the length of the message in bytes */
	       byte_length = MPITEST_get_message_length(length_count);
	       /* the length of the buffer in elements of the current type */
	       length = MPITEST_byte_to_element(test_type, byte_length);
	       if (length > max_length)
		  length = max_length;	/* rev2 */
	       
	       
	       if (MPITEST_current_rank == 0) {
		  sprintf(info_buf, "length %d commsize %d commtype %d data_type %d",
			  length, test_nump, comm_type, test_type);
		  MPITEST_message(MPITEST_INFO1, info_buf);
	       }
	       
	       /* Set up the dataTemplate for initializing send buffer */
	       /*
		* send buffer is initialized to ((sender rank)*1000+rec'ver
		* rank)
		*/
	       for (i = 0; i < test_nump; i++) {
		  counts[i] = length;
		  displs[i] = i * length;
		  MPITEST_dataTemplate_init(&values[i],
					    1000 * MPITEST_current_rank + i);
	       }
	       /* Initialize send buffer */
	       MPITEST_init_buffer_v(test_type, test_nump, counts, displs,
				     values, send_buffer);


	       /* set up data Template for initializing the recv buffer */
	       MPITEST_dataTemplate_init(&value, -1);

	       /* initialize the recv buffer */
	       MPITEST_init_buffer(test_type, length * test_nump, value,
				   recv_buffer);


	       /* set up the dataTemplate for error checking     */
	       /* For the recv'ed messages, the value should be  */
	       /* (MPITEST_current_rank+1000*sender)             */
	       for (i = 0; i < test_nump; i++) {
		  MPITEST_dataTemplate_init(&values[i],
					    MPITEST_current_rank + i * 1000);
	       }

	       /* execute the function under test */
	       loop_cnt++;
	       ierr = MPI_Alltoall(send_buffer, length,
				   MPITEST_mpi_datatypes[test_type],
				   recv_buffer, length,
				   MPITEST_mpi_datatypes[test_type],
				   comm);

	       if (ierr != MPI_SUCCESS) {
		  sprintf(info_buf, "MPI_Alltoall() returned %d", ierr);
		  MPITEST_message(MPITEST_NONFATAL, info_buf);
		  MPI_Error_string(ierr, error_string, &size);
		  MPITEST_message(MPITEST_FATAL, error_string);
		  fail++;
	       }

	       error = MPITEST_buffer_errors_v(test_type, test_nump, counts,
					       displs, values, recv_buffer);

	       if (error) {
		  if (ierr == MPI_SUCCESS)
		     fail++;
		  sprintf(info_buf, "%d errors in buffer, len %d commsize %d commtype %d data_type %d",
			  error, length, test_nump, comm_type, test_type);
		  MPITEST_message(MPITEST_NONFATAL, info_buf);
	       }
	       else {
		  sprintf(info_buf, "%d errors found in buffer", error);
		  MPITEST_message(MPITEST_INFO2, info_buf);
	       }

	    }			/***** if (MPITEST_current_rank != ...) **********/
	    free(send_buffer);
	    free(recv_buffer);

	 }			/***** for (length_count=0;...) ********/
	 free(counts);
	 free(displs);
	 free(values);

      }				/***** for (type_count=0;...) ********/

      MPITEST_free_communicator(comm_type, &comm);

   }				/***** for (comm_count=0;...) *******/
   /* report overall results  */
   MPITEST_report(loop_cnt - fail, fail, 0, testname);

   MPI_Finalize();
   return 0;
}				/***** main() ********/
