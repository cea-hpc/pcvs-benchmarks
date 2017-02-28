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
                          Test for MPI_Test_cancelled()

The root rank will submit Isend() to the next rank (circular).
The root rank will then do a MPI_Wait() for the request to be completed.
It then call MPI_Test_cancelled to verify that the send request has NOT
been cancelled.  Depending on whether MPITEST_STATUS_CHK is #defined or
not, the returning status object of MPI_Recv() may or may not be verified.

The receiving rank will then do a blocking receive followed by a
MPI_Test_cancelled() making sure that the receive request has also NOT been
cancelled.  Depending on whether MPITEST_STATUS_CHK is #defined or not, the
returning status object of MPI_Recv() may or may not be verified.
Depending on whether MPITEST_BUFFER_CHK is #defined or not, the recv buffer
may not may not be verified.

All participating ranks will then do a MPI_Barrier().

This test may be run in any communicator with a minimum of 2 group members.

The MPITEST environment provides looping over communicator size, message
length, and root's rank.

The properties of the loops are encoded in configuration arrays in the
file mpitest_cfg.h .

MPI Calls dependencies for this test:
  MPI_Isend), MPI_Test_cancelled(), MPI_Recv(), MPI_Init(),
  MPI_Finalize(), MPI_Comm_test_inter(), MPI_Barrier(), MPI_Error_string(),
  MPI_Wait(), [MPI_Get_count(), MPI_Allreduce(), MPI_Comm_rank(), MPI_Comm_size()]
******************************************************************************/

#include "mpitest_cfg.h"
#include "mpitest.h"

/* Tag to be used in transmission */
#define MPITEST_TAG  119

int main(int argc, char *argv[])
{
   int
     test_type,         /*  the index of the current buffer type              */
     length,            /*  The length of the current buffer                  */
     byte_length,       /*  The length of the current buffer in bytes         */
     test_nump,         /*  The number of processors in current communicator  */
     comm_index,        /*  the array index of the current comm               */
     comm_type,         /*  the index of the current communicator type        */
     type_count,        /*  loop counter for data type loop                   */
     length_count,      /*  loop counter for message length loop              */
     comm_count,        /*  loop counter for communicator loop                */
     error,             /*  errors from one MPI call                          */
     fail,              /*  counts total number of failures                   */
     size,              /*  return size from MPI_Error_string                 */
     loop_cnt,          /*  counts total number of loops through test         */
     ierr,              /*  return value from MPI calls                       */
     max_length,        /*  maximum buffer length specified in config. file   */
     max_byte_length,   /*  maximum buffer length in bytes                    */
     root,              /*  the root of the current broadcast                 */
     dest,              /*  destination rank of where message is being sent   */
     tag = MPITEST_TAG, /*  message tag                                       */
     i, j;

#ifdef MPITEST_STATUS_CHK
   int count;           /*  length of message to be received from MPI_Get_count() */
#endif

   struct dataTemplate
     value;             /*  dataTemplate for initializing buffers             */
   struct dataTemplate
     *values;           /*  Array of dataTemplates for verbose init           */

   void *buffer;        /* message buffer                          */

   char
     info_buf[256],     /*  buffer for passing mesages to MPITEST             */
     testname[128];     /*  the name of this test                             */
    char error_string[MPI_MAX_ERROR_STRING];

   MPI_Comm comm;       /*  MPI communicator                                  */

   MPI_Status status;

   MPI_Request send_request;

   int inter_flag, flag;

   ierr = MPI_Init(&argc, &argv);
   if (ierr!=MPI_SUCCESS) {
      sprintf(info_buf, "MPI_Init() returned %d", ierr);
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

   sprintf(testname, "MPI_Test_cancelled_false");

   MPITEST_init(argc, argv);

   if (MPITEST_me==0) {
      sprintf(info_buf, "Starting %s test", testname);
      MPITEST_message(MPITEST_INFO0, info_buf);
   }

   /* set the global error counter */
   fail = 0;
   loop_cnt = 0;

   /* find the maximum sized buffer we will use */
   max_byte_length = MPITEST_get_max_message_length();

   for (comm_count=0; comm_count<MPITEST_num_comm_sizes();comm_count++) {
      comm_index = MPITEST_get_comm_index(comm_count);
      comm_type = MPITEST_get_comm_type(comm_count);

      test_nump = MPITEST_get_communicator(comm_type, comm_index, &comm);

      if (test_nump < 2) {
	 /* Skipping communicator with comm size < 2 */
	 sprintf(info_buf, "Skipping communicator with comm_size < 2 (commtype: %d) for this test", comm_type);
	 MPITEST_message(MPITEST_INFO1, info_buf);
	 continue;
      }

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
	    /* Ignore inter-communicator for test */
	    MPITEST_free_communicator(comm_type, &comm);
	    sprintf(info_buf, "Skipping inter communicator (commtype: %d) for this test", comm_type);
	    continue;
	 }
	 
	 /* Allocate request arrays */
	 
	 test_type = MPITEST_int;
	 
	 /* convert the number of bytes in the maximum length message */
	 /*  into the number of elements of the current type */
	 max_length = MPITEST_byte_to_element(test_type, max_byte_length);
	 
	 /* then allocate the buffer */
	 MPITEST_get_buffer(test_type, max_length, &buffer);
	 
	 for (length_count=0;length_count<MPITEST_num_message_lengths();length_count++) {
	    byte_length = MPITEST_get_message_length(length_count);
	    length = MPITEST_byte_to_element(test_type, byte_length);
	    
	    for (root=0; root<test_nump; root++) {
	       /* print an informational message */
	       if (MPITEST_current_rank==0) {
		  sprintf(info_buf, "(%d, %d) length %d commsize %d commtype %d data_type %d root %d",
			  length_count, comm_count, length,
			  test_nump, comm_type, test_type, root);
		  MPITEST_message(MPITEST_INFO1, info_buf);
	       }
	       
	       /* Set up the dataTemplate for initializing send buffer */
	       /* Initialize send buffer */
	       MPITEST_dataTemplate_init(&value, MPITEST_current_rank);

	       MPITEST_init_buffer(test_type, length+1,
					 value, buffer);

	       loop_cnt++;

	       /* Destination rank the message is being sent to */
	       dest = root + 1;

	       if (dest >= test_nump) {
		  dest = 0;
	       }

	       if (MPITEST_current_rank == root) {

		  sprintf(info_buf, "Sending message from source %d to %d, tag: %d", root, dest, tag);
		  MPITEST_message(MPITEST_INFO1, info_buf);

		  ierr = MPI_Isend(buffer, length, MPITEST_mpi_datatypes[test_type],
				   dest, tag, comm, &send_request);
		  if (ierr != MPI_SUCCESS) {
		     fail++;
		     sprintf(info_buf, "MPI_Isend() returned %d", ierr);
		     MPITEST_message(MPITEST_NONFATAL, info_buf);
		     MPI_Error_string(ierr, error_string, &size);
		     MPITEST_message(MPITEST_FATAL, error_string);
		  }
		  else {
		     sprintf(info_buf, "Waiting for send request to be completed");
		     MPITEST_message(MPITEST_INFO1, info_buf);

		     ierr = MPI_Wait(&send_request, &status);

		     if (ierr != MPI_SUCCESS) {
			fail++;
			sprintf(info_buf, "MPI_Wait() returned %d", ierr);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
			MPI_Error_string(ierr, error_string, &size);
			MPITEST_message(MPITEST_FATAL, error_string);
		     }

		     sprintf(info_buf, "Checking if the send request has been cancelled");
		     MPITEST_message(MPITEST_INFO1, info_buf);

		     ierr = MPI_Test_cancelled(&status, &flag);

		     if (ierr != MPI_SUCCESS) {
			fail++;
			sprintf(info_buf, "MPI_Test_cancelled() returned %d", ierr);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
			MPI_Error_string(ierr, error_string, &size);
			MPITEST_message(MPITEST_FATAL, error_string);			   
		     }
		     else if (flag) {
			fail++;
			sprintf(info_buf, "MPI_Test_cancelled() return non-zero flag with a completed send request");
			MPITEST_message(MPITEST_NONFATAL, info_buf);
		     }
		  }
	       }
	       else if (MPITEST_current_rank == dest) {
		  ierr = MPI_Recv(buffer, length, MPITEST_mpi_datatypes[test_type],
				  root, tag, comm, &status);

		  if (ierr != MPI_SUCCESS) {
		     fail++;
		     sprintf(info_buf, "MPI_Recv() returned %d", ierr);
		     MPITEST_message(MPITEST_NONFATAL, info_buf);
		     MPI_Error_string(ierr, error_string, &size);
		     MPITEST_message(MPITEST_FATAL, error_string);
		  }
		  else {
		     sprintf(info_buf, "Checking if the recv request has been cancelled");
		     MPITEST_message(MPITEST_INFO1, info_buf);

		     ierr = MPI_Test_cancelled(&status, &flag);

		     if (ierr != MPI_SUCCESS) {
			fail++;
			sprintf(info_buf, "MPI_Test_cancelled() returned %d", ierr);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
			MPI_Error_string(ierr, error_string, &size);
			MPITEST_message(MPITEST_FATAL, error_string);			   
		     }
		     else if (flag) {
			fail++;
			sprintf(info_buf, "MPI_Test_cancelled() return non-zero flag with a completed recv request");
			MPITEST_message(MPITEST_NONFATAL, info_buf);
		     }
		     else {
#ifdef MPITEST_STATUS_CHK
			/* Check status.MPI_SOURCE */

			sprintf(info_buf, "Verifying output status object from MPI_Recv()");
			MPITEST_message(MPITEST_INFO1, info_buf);

			if (status.MPI_SOURCE != root) {
			   fail++;
			   sprintf(info_buf,
				   "status object returned from MPI_Recv() has unexpected MPI_SOURCE (%d) field",
				   status.MPI_SOURCE);
			   MPITEST_message(MPITEST_NONFATAL, info_buf);
			}

			/* Check status.MPI_TAG */
			if (status.MPI_TAG != tag) {
			   fail++;
			   sprintf(info_buf,
				   "status object returned from MPI_Recv() has unexpected MPI_TAG"
				   "  Ecpected: %d, Actual: %d", tag, status.MPI_TAG);
			   MPITEST_message(MPITEST_NONFATAL, info_buf);
			}

			/* Not checking status.MPI_ERROR */

			/* Check the length of the message to be received */
			ierr = MPI_Get_count(&status, MPITEST_mpi_datatypes[test_type], &count);
			if (ierr != MPI_SUCCESS) {
			   fail++;
			   sprintf(info_buf, "MPI_Get_count() returned %d", ierr);
			   MPITEST_message(MPITEST_NONFATAL, info_buf);
			   MPI_Error_string(ierr, error_string, &size);
			   MPITEST_message(MPITEST_FATAL, error_string);
			}
			else {
			   if (count != length) {
			      fail++;
			      sprintf(info_buf, "status object returned from MPI_Recv() contains unexpected length."
				      "  Expected: %d, Actual: %d", length, count);
			      MPITEST_message(MPITEST_NONFATAL, info_buf);
			   }
			}
#else
			sprintf(info_buf, "output status object from MPI_Recv() *not* verified");
			MPITEST_message(MPITEST_INFO1, info_buf);
#endif

#ifdef MPITEST_BUFFER_CHK
			sprintf(info_buf, "Verifying data received");
			MPITEST_message(MPITEST_INFO1, info_buf);
   
			/* Expecting to receive the root's rank number from root */
			MPITEST_dataTemplate_init(&value, root);
			error = MPITEST_buffer_errors(test_type, length, value, buffer);

			if (error) {
			   if (ierr == MPI_SUCCESS) fail++;
			   sprintf(info_buf, "%d errors in buffer (%d, %d) len %d commsize %d commtype %d data_type %d root %d",
				   error, length_count, comm_count, length, test_nump, comm_type, test_type, root);
			   MPITEST_message(MPITEST_NONFATAL, info_buf);
			} else {
			   sprintf(info_buf,"%d errors found in buffer", error);
			   MPITEST_message(MPITEST_INFO2, info_buf);
			}
#else
			sprintf(info_buf, "data received *not* verified");
			MPITEST_message(MPITEST_INFO1, info_buf);
#endif

		     }
		  }		    	  
	       }

	       ierr = MPI_Barrier(comm);
	       if (ierr != MPI_SUCCESS) {
		  fail++;
		  sprintf(info_buf, "MPI_Barrier() returned %d", ierr);
		  MPITEST_message(MPITEST_NONFATAL, info_buf);
		  MPI_Error_string(ierr, error_string, &size);
		  MPITEST_message(MPITEST_FATAL, error_string);
	       }
	    }
	 }
         free(buffer);      
      }

      MPITEST_free_communicator(comm_type, &comm);
   }
   /* report overall results  */
   MPITEST_report(loop_cnt-fail, fail, 0, testname);

   MPI_Finalize();
   return 0;
} /* main() */

