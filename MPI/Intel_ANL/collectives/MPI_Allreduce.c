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
                       Test for MPI_Allreduce()

This code tests the MPI_Allreduce() function.  MPI_Allreduce() performs a
binary, associative operation in such a way that data from process "i" is
operand exactly once.

In addition to the normal loops provided by the MPITEST environment (message
length, communicator size and type, and data type) this test also loops over
the operation to be used in the reduce call.

The operations to be looped over are stored in the array MPITEST_default_ops[].
This array must be initialized at runtime, after the call to MPI_Init().

There are several auxiliary functions needed for Reduce tests, and these
are in the module reduce.c, with its associated header file reduce.h .

******************************************************************************/

#include "mpitest_cfg.h"
#include "mpitest.h"
#include "reduce.h"

extern struct MPITEST_op MPITEST_reduce_ops[];


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
      num_ops,			/* total number of predefined MPI ops                 */
      op_count, i, j;

   struct dataTemplate
      value;			/* dataTemplate for initializing buffers             */

   void *send_buffer,		/* message buffer                                     */
     *recv_buffer;


   char
      info_buf[256],		/* buffer for passing mesages to MPITEST             */
      testname[128];		/* the name of this test                             */
    char error_string[MPI_MAX_ERROR_STRING];


   MPI_Comm comm;		/* MPI communicator                                  */

   MPI_Op MPITEST_default_ops[OP_ARRAY_SIZE];

   int inter_flag;

   ierr = MPI_Init(&argc, &argv);
   if (ierr != MPI_SUCCESS) {
      sprintf(info_buf, "MPI_Init() returned %d", ierr);
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

   sprintf(testname, "MPI_Allreduce()");

   num_ops = set_default_ops(MPITEST_default_ops);

   MPITEST_init(argc, argv);

   if (MPITEST_me == 0) {
      sprintf(info_buf, "Starting %s test", testname);
      MPITEST_message(MPITEST_INFO0, info_buf);
   }

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
	 for (type_count = 0; type_count < MPITEST_num_datatypes(); type_count++) {
	    test_type = MPITEST_get_datatype(type_count);
	    
	    /* find the maximum sized buffer we will use */
	    max_byte_length = MPITEST_get_max_message_length();
	    
	    /*
	     * convert the number of bytes to the number of elements of the
	     * current type
	     */
	    max_length = MPITEST_byte_to_element(test_type, max_byte_length);
	    
	    /* then allocate the buffer */
	    MPITEST_get_buffer(test_type, max_length, &send_buffer);
	    MPITEST_get_buffer(test_type, max_length, &recv_buffer);
	    
	    for (length_count = 0; length_count < MPITEST_num_message_lengths(); length_count++) {
	       byte_length = MPITEST_get_message_length(length_count);
	       length = MPITEST_byte_to_element(test_type, byte_length);
	       
	       for (op_count = 0; op_count < num_ops; op_count++) {
		  if (MPITEST_current_rank == 0) {
		     sprintf(info_buf, "length %d commsize %d commtype %d data_type %d operation %d",
			     length, test_nump, comm_type, test_type, op_count);
		     MPITEST_message(MPITEST_INFO1, info_buf);
		  }

		  /* Set up the dataTemplate for initializing send buffer */
		  if (MPITEST_default_ops[op_count] == MPI_PROD) {
		     j = (MPITEST_current_rank + 1) / 2;
		     j = j * 2 - MPITEST_current_rank + 1;
		     MPITEST_dataTemplate_init(&value, j);
		  }
		  else {
		     MPITEST_dataTemplate_init(&value, MPITEST_current_rank + 1);
		  }
		  
		  /* Initialize send buffer */
		  MPITEST_init_buffer(test_type, length, value, send_buffer);
		  
		  /* Set up the dataTemplate for initializing recv buffer */
		  MPITEST_dataTemplate_init(&value, -1);

		  /* Initialize receive buffer */
		  MPITEST_init_buffer(test_type, length + 1,
				      value, recv_buffer);


		  if (has_op(op_count, test_type) && (length != 0)) {
		     loop_cnt++;
		     ierr = MPI_Allreduce(send_buffer, recv_buffer, length,
					  MPITEST_mpi_datatypes[test_type],
					  MPITEST_default_ops[op_count],
					  comm);

		     if (ierr != MPI_SUCCESS) {
			sprintf(info_buf, "MPI_Allreduce() returned %d", ierr);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
			MPI_Error_string(ierr, error_string, &size);
			MPITEST_message(MPITEST_FATAL, error_string);
			fail++;
		     }

		     /* generate the correct answer */
		     get_reduce_answer(op_count, test_nump, &value);

		     /* error test */
		     error = 0;
		     error = MPITEST_buffer_errors(test_type, length,
						   value, recv_buffer);

		     /* check for recv_buffer overflow */
		     MPITEST_dataTemplate_init(&value, -1);
		     error += MPITEST_buffer_errors_ov(test_type,
						length, value, recv_buffer);

		     if (error) {
			if (ierr == MPI_SUCCESS)
			   fail++;
			sprintf(info_buf, "%d errors in buffer, len %d commsize %d commtype %d data_type %d op %d",
			     error, length, test_nump, comm_type, test_type,
				op_count);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
		     }
		     else {
			sprintf(info_buf, "%d errors found in buffer", error);
			MPITEST_message(MPITEST_INFO2, info_buf);
		     }
		  }
	       }
	    }			/**** for (op_count=0;...) *******/

	    free(send_buffer);
	    free(recv_buffer);
	 }			/**** for (length=0;...) *******/

      }				/**** for (type=0;...) *******/

      MPITEST_free_communicator(comm_type, &comm);

   }				/**** for (comm=0;...) *******/
   /* report overall results  */
   MPITEST_report(loop_cnt - fail, fail, 0, testname);

   MPI_Finalize();
   return 0;
}				/* main() */
