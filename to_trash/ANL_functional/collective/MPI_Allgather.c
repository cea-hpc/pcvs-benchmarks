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
                     Test for MPI_Allgather()

The MPI_Allgather() function causes a piece of data from each process
in a group to be sent to all other processes in the group.  Each
process concatenates the received data in rank order.

This code tests Allgather in the following way.  Each process initializes its
send buffer with its own rank (or an appropriate generalization for
non-integer types).  After the Allgather returns, each process checks
to make sure that the i_th chunk of data in its receive buffer
contains the value initialized by the i_th process.

This test may be run on a communicator of any size and type, with data
of any type which MPI allows, and with message lengths of any
non-negative integer.  The MPITEST environment provides looping over
each of these attributes, in a way defined in the file mpitest_cfg.h .  See
the MPITEST README for more details.

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
      max_length,		/* maximum buffer length specified in mpitest_cfg.h
				 * file   */
      max_byte_length,		/* max buffer length in bytes                        */
      i, j;

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
      testname[128];		/* the name of the current MPI function              */
    char error_string[MPI_MAX_ERROR_STRING];

   MPI_Comm comm;		/* MPI communicator                                  */

   int inter_flag;


   ierr = MPI_Init(&argc, &argv);
   if (ierr != MPI_SUCCESS) {
      sprintf(info_buf, "MPI_Init() returned %d", ierr);
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

   sprintf(testname, "MPI_Allgather()");

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

	 if (inter_flag) {		/* Ignore inter-communicator for collective
					 * functional tests */
	    MPITEST_free_communicator(comm_type, &comm);
	    sprintf(info_buf, "Skipping intercommunicator (commtype: %d) for this test", comm_type);
	    MPITEST_message(MPITEST_INFO1, info_buf);

	    continue;
	 }

	 max_byte_length = MPITEST_get_max_message_length();


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

	    MPITEST_get_buffer(test_type, max_length * MPITEST_nump, &recv_buffer);
	    MPITEST_get_buffer(test_type, max_length, &send_buffer);

	    for (length_count = 0; length_count < MPITEST_num_message_lengths(); length_count++) {
	       byte_length = MPITEST_get_message_length(length_count);
	       length = MPITEST_byte_to_element(test_type, byte_length);

	       if (length > max_length)
		  length = max_length;	/* rev2 */

	       if (MPITEST_current_rank == 0) {
		  sprintf(info_buf, "(%d,%d,%d) length %d commsize %d commtype %d data_type %d",
			  length_count, comm_count, type_count, length, test_nump,
			  comm_type, test_type);
		  MPITEST_message(MPITEST_INFO1, info_buf);
	       }

	       /* Set up the dataTemplate for initializing recv buffer */
	       MPITEST_dataTemplate_init(&value, -1);

	       /* Initialize recv buffer */
	       MPITEST_init_buffer(test_type, test_nump * length,
				   value, recv_buffer);


	       /* Set up the dataTemplate for initializing send buffer */
	       MPITEST_dataTemplate_init(&value, MPITEST_current_rank);

	       /* Initialize send buffer */
	       MPITEST_init_buffer(test_type, length,
				   value, send_buffer);


	       /* Set up arrays for error testing  */
	       for (i = 0; i < test_nump; i++) {
		  counts[i] = length;
		  displs[i] = length * i;

		  MPITEST_dataTemplate_init(&values[i], i);
	       }

	       loop_cnt++;
	       ierr = MPI_Allgather(send_buffer, length,
				    MPITEST_mpi_datatypes[test_type],
				    recv_buffer, length,
				    MPITEST_mpi_datatypes[test_type],
				    comm);

	       if (ierr != MPI_SUCCESS) {
		  sprintf(info_buf, "MPI_Allgather returned %d", ierr);
		  MPITEST_message(MPITEST_NONFATAL, testname);
		  MPI_Error_string(ierr, error_string, &size);
		  MPITEST_message(MPITEST_FATAL, error_string);
		  fail++;
	       }

	       error = MPITEST_buffer_errors_v(test_type, test_nump, counts,
					       displs, values, recv_buffer);


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
	    }
	    free(recv_buffer);
	    free(send_buffer);

	 }			/****** for (length_count=0;...) *********/
	 free(counts);
	 free(displs);
	 free(values);
      }				/****** for (type_count=0;...) *********/

      MPITEST_free_communicator(comm_type, &comm);

   }				/****** for (comm_count=0;...) *********/

   /* report overall results  */
   MPITEST_report(loop_cnt - fail, fail, 0, testname);

   ierr = MPI_Finalize();

   return 0;
}				/* main() */
