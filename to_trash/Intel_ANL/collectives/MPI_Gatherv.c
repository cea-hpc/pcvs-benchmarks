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
                         Test for MPI_Gatherv()

This code tests the functionality of the MPI_Gatherv() function.
This function extends the functionality of MPI_Gather() (to gather
a piece of data from each node to one root node) so that the pieces
of data from different nodes may be different sizes for different
nodes, and so that the displacement from the beginning of the receive
buffer may leave spaces in between sections of data.

The latter feature is useful if one is sending data stored as a matrix
with a leading dimension.  In this case, the beginning of the ith
column is at location buffer[i*leading_dim], and one might want to
store matrices whose columns' lengths are less than the leading
dimension.  In that case, if one wanted to do a Gather() of
columns of the matrix, one would use MPI_Gatherv().

This test uses the standard MPITEST library functions to generate the
message lengths, communicator sizes and types, and buffer types.  Once
the environment is set up, the variable leading_dim is set to length
plus some constant value, then the buffers are allocated accordingly
(send_buffer has size "length", and recv_buffer has size
"test_nump*leading_dim".)  The receive buffer is initialized to either
-1, 999, or 'z' (depending on the buffer type in use), and the send
buffer is initialized to something unique to the process (the process'
rank in the case of the integer types, the rank plus a small real part
in the case of floats and doubles, and a character representation of
the rank in the case of the character types.)

Several subsidiary arrays are necessary for this test.  The arrays
recv_counts[] and recv_displs[] list the message length and starting
displacement in the receive array of the different process's messages.
The arrays counts[] and displs[] and values[] are used for error
checking.  counts[2*i] is equal to recv_counts[i] (i.e. the length of
the message from the ith process), but counts[2*i+1] is equal to the
length of the space in the receive buffer between the message from the
ith process and that from the (i+1)th process.  In this test,
counts[2*i+1]=(leading_dim-length) for all 0 < i < test_nump.
Similarly, displs[2*i] is equal to recv_displs[i] (the displacement of
the ith message from the beginning of the receive buffer), and
displs[2*i+1] holds the displacement of the space between the ith and
(i+1)th messages.  The array values[] holds the correct values which
should be in the receive buffer upon succesful completion of this
test.  values[2*i] holds the same value as the send buffer on the ith
process, and values[2*i+1] holds the value which was in the receive
buffer before the call to MPI_Gatherv().  Note that values[] is an
array of type dataTemplate, so each element must be filled with all
ten possible types of value before it is used.

Revision History:
  1               gm   Original version.
  2     2/1/96    gt   Reduced size of send buffer to stay within bounds,
                       and modified data sent/expected.
******************************************************************************/

#include "mpitest_cfg.h"
#include "mpitest.h"

/*
 * This is the amount of blank space to leave at the end of each incoming
 * buffer.  This mimics the storage of matrices with leading dimension.  In
 * this case, the leading dimension is the message length plus EXTRA_SPACE.
 */
#define EXTRA_SPACE 1

int main(int argc, char *argv[])
{
   int
      test_type,		/* the index of the current buffer type              */
      length,			/* The length of the current buffer                  */
      max_length,		/* the largest buffer length                         */
      byte_length,		/* buffer length in bytes                            */
      max_byte_length,		/* maximum buffer length in bytes                    */
      test_nump,		/* The number of processors in current
				 * communicator  */
      comm_size,		/* Equal to test_nump                                */
      comm_index,		/* index of current comm's size token                */
      comm_type,		/* the index of the current communicator type        */
      type_count,		/* loop counter for data type loop                   */
      length_count,		/* loop counter for message length loop              */
      comm_count,		/* loop counter for communicator loop                */
      error,			/* current number of errors                          */
      i, j,			/* miscellaneous counter variables                   */
      leading_dim,		/* leading dimension for _v option                   */
      fail = 0,			/* total number of failures this run                 */
      size,			/* return size from MPI_Error_string                 */
      loop_cnt = 0,		/* counts total number of loops through test         */
      root,			/* the current gather root                           */
      ierr;			/* return value from MPI functions                   */


   struct dataTemplate
      value;			/* dataTemplate for initializing buffers             */
   struct dataTemplate
     *values;			/* Array of dataTemplates for verbose init           */

   void *recv_buffer,		/* buffers for sending and receiving                 */
     *send_buffer;

   int				/* for verbose initialization and checking           */
     *counts,			/* the array of item counts                       */
     *recv_counts, *displs,	/* the array of displacements from buffer[0]      */
     *recv_displs;

   char
      info_buf[256],		/* buffer for MPITEST system messages                */
      testname[128];		/* the name of this test                             */
    char error_string[MPI_MAX_ERROR_STRING];


   MPI_Comm comm;		/* MPI communicator                                  */

   int inter_flag;

   ierr = MPI_Init(&argc, &argv);
   if (ierr != MPI_SUCCESS) {
      sprintf(info_buf, "MPI_Init() returned %d", ierr);
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

   sprintf(testname, "MPI_Gatherv()");

   MPITEST_init(argc, argv);

   if (MPITEST_me == 0) {
      sprintf(info_buf, "Starting %s test", testname);
      MPITEST_message(MPITEST_INFO0, info_buf);
   }

   /* loop over the communicators */
   for (comm_count = 0; comm_count < MPITEST_num_comm_sizes(); comm_count++) {
      /*
       * get communicator size and type for this communicator loop iteration
       */
      comm_index = MPITEST_get_comm_index(comm_count);
      comm_type = MPITEST_get_comm_type(comm_count);

      /* get the communicator */
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
	 /* Buffers for error checking */
	 counts = (int *) malloc(2 * test_nump * sizeof(int));
	 recv_counts = (int *) malloc(test_nump * sizeof(int));
	 displs = (int *) malloc(2 * test_nump * sizeof(int));
	 recv_displs = (int *) malloc(test_nump * sizeof(int));
	 values = (struct dataTemplate *) malloc(2 * test_nump * sizeof(struct dataTemplate));
	 if (!counts || !displs || !values || !recv_counts || !recv_displs)
	    MPITEST_message(MPITEST_FATAL, "Couldn't allocate test arrays");
	 
	 
	 /* the buffer type loop */
	 for (type_count = 0; type_count < MPITEST_num_datatypes(); type_count++) {
	    /* get the buffer type */
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
	    
	    
	    /* then allocate the buffer */
	    /*
	     * Get the send and recv buffers.  The recv buffer must be test_nump
	     * times as long as the send buffer for this particular test, since
	     * each rank receives from every other rank.  Also, since this test
	     * is testing the "v" version (MPI_Allgatherv()), we give the recv
	     * buffer some extra space as if we were receiving a matrix stored
	     * with a leading dimension larger than its comumn length.
	     */
	    
	    MPITEST_get_buffer(test_type, (max_length + EXTRA_SPACE), &send_buffer);
	    MPITEST_get_buffer(test_type, (max_length + EXTRA_SPACE) * MPITEST_nump,
			       &recv_buffer);
	    
	    
	    /* loop over the message lengths */
	    for (length_count = 0; length_count < MPITEST_num_message_lengths(); length_count++) {
	       for (root = 0; root < test_nump; root++) {
		  /* set the message length */
		  byte_length = MPITEST_get_message_length(length_count);
		  length = MPITEST_byte_to_element(test_type, byte_length);
		  if (length > max_length)
		     length = max_length;	/* rev2 */
		  
		  /*
		   * This is the size of the displacements between messages
		   * recv'd on a particular process.
		   */
		  leading_dim = length + EXTRA_SPACE;
		  
		  
		  /* print an informational message */
		  if (MPITEST_current_rank == 0) {
		     sprintf(info_buf, "(%d,%d,%d) length %d commsize %d commtype %d data_type %d",
			     length_count, comm_count, type_count, length,
			     test_nump, comm_type, test_type);
		     MPITEST_message(MPITEST_INFO1, info_buf);
		  }
		  
		  /*
		   * Set up dataTemplate for initializing recv buffer.  These
		   * will be the values before the send is complete, and the
		   * values for in between spaces after the send completes.
		   */
		  MPITEST_dataTemplate_init(&value, -1);
		  
		  /*
		   * initialize the receive buffer.  The error checking will
		   * make sure this value is in the spaces between messages.
		   */
		  MPITEST_init_buffer(test_type, test_nump * leading_dim,
				      value, recv_buffer);
		  
		  
		  /* Set up the dataTemplate for initializing send buffer */
		  MPITEST_dataTemplate_init(&value, MPITEST_current_rank);

		  /* Initialize the send buffer */
		  MPITEST_init_buffer(test_type, length + 1,
				      value, send_buffer);


		  /* Set up arrays for error testing  */
		  for (i = 0, j = 0; i < test_nump; i++, j = 1 - j) {
		     /* the beginning of the ith buffer */
		     displs[2 * i] = i * leading_dim;

		     /*
		      * the beginning of the space in between the ith and
		      * (i+1)th buffers
		      */
		     displs[(2 * i) + 1] = displs[2 * i] + length + j;

		     /* the length of the ith buffer */
		     counts[2 * i] = length + j;

		     /*
		      * the length of the space between the ith and i+1th
		      * buffers
		      */
		     counts[(2 * i) + 1] = leading_dim - length - j;


		     /* the length of the ith buffer */
		     recv_counts[i] = length + j;

		     /* the beginning of the ith buffer */
		     recv_displs[i] = i * leading_dim;

		     /* the value in the ith buffer */
		     MPITEST_dataTemplate_init(&values[2 * i], i);

		     /* the value in between the ith and (i+1)th buffer */
		     MPITEST_dataTemplate_init(&values[2 * i + 1], -1);

		  }

		  loop_cnt++;
		  j = MPITEST_current_rank % 2;
		  ierr = MPI_Gatherv(send_buffer, length + j,
				     MPITEST_mpi_datatypes[test_type],
				     recv_buffer, recv_counts, recv_displs,
				     MPITEST_mpi_datatypes[test_type], root,
				     comm);

		  if (ierr != MPI_SUCCESS) {
		     fail++;
		     sprintf(info_buf, "MPI_Gatherv() returned %d", ierr);
		     MPITEST_message(MPITEST_NONFATAL, info_buf);
		     MPI_Error_string(ierr, error_string, &size);
		     MPITEST_message(MPITEST_FATAL, error_string);
		  }


		  /* error checking */
		  error = 0;
		  if (MPITEST_current_rank == root) {
		     error = MPITEST_buffer_errors_v(test_type, 2 * test_nump,
				       counts, displs, values, recv_buffer);
		  }

		  if (error) {
		     if (ierr == MPI_SUCCESS)
			fail++;
		     sprintf(info_buf, "%d errors in buffer (%d,%d,%d) len %d commsize %d commtype %d data_type %d",
			error, length_count, comm_count, type_count, length,
			     test_nump, comm_type, test_type);
		     MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
		  else {
		     sprintf(info_buf, "%d errors found in buffer", error);
		     MPITEST_message(MPITEST_INFO2, info_buf);
		  }

		  sprintf(info_buf, "Node %d had %d errors in %s data",
			  MPITEST_me, error, testname);
		  MPITEST_message(MPITEST_INFO2, info_buf);
	       }


	    }			/**************for (root=0;...) **************/

	    free(recv_buffer);
	    free(send_buffer);
	 }			/********** for (length_count=0;...) ********/

	 /* free memory */
	 free(counts);
	 free(displs);
	 free(recv_counts);
	 free(recv_displs);
	 free(values);

      }				/********** for (type_count=0;...) ********/

      /* free the communicator */
      MPITEST_free_communicator(comm_type, &comm);

   }				/********** for (comm_count=0;...) ********/

   MPITEST_report(loop_cnt - fail, fail, 0, testname);

   /* finalize the MPI environment */
   MPI_Finalize();
   return 0;
}				/* main() */
