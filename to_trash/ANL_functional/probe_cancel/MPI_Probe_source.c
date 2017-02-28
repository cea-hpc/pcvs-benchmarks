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
                          Test for MPI_Probe()

Each rank (except the root rank) will do a send to the root rank.  The root
rank will then do a MPI_Probe() followed by a MPI_Recv().  The buffer
received may or may not be send and / or checked (depending on if
MPITEST_BUFFER_RECV and / or MPITEST_BUFFER_CHK is / are #defined or not in
this source).  The status object returned from MPI_Probe() may or may not
be checked depending on if MPITEST_STATUS_CHK is defined or not.

This test may be run in any communicator with a minimum of 2 group members,
with any data type, and with any non-negative message length.

The MPITEST environment provides looping over communicator size and
message length (if MPITEST_STATUS_CHK is #defined)  The properties of the
loops are encoded in configuration arrays in the file mpitest_cfg.h .

MPI Calls dependencies for this test:
  MPI_Isend(), MPI_Recv(), MPI_Cancel(), MPI_Probe(), MPI_Init(), MPI_Finalize()
  MPI_Comm_test_inter(), MPI_Barrier(), MPI_Error_string(), 
  [MPI_Get_count(), MPI_Allreduce(), MPI_Comm_rank(), MPI_Comm_size()]
******************************************************************************/

#include "mpitest_cfg.h"
#include "mpitest.h"

/* Tag to be used for message transmission in this test            */
#define MPITEST_TAG           1

/* Maximum number of processes to participate in message sending   */
/* This is here so that the test can be run in arbitray number     */
/* of MPI processess without potentially exhausting message buffer */
/* as the number of mpi processes created increase.                */ 
#define MPITEST_MAX_NP        4

/* Comment the following #define out to turn off receiving buffer */
#define MPITEST_BUFFER_RECV   1

int main(int argc, char *argv[])
{
   int
     test_type,         /*  the index of the current buffer type              */
     length_count,      /*  loop counter for length loop                      */
     length,            /*  The length of the current buffer                  */
     byte_length,       /*  The length of the current buffer in bytes         */
     test_nump,         /*  The number of processors in current communicator  */
     comm_index,        /*  the array index of the current comm               */
     comm_type,         /*  the index of the current communicator type        */
     comm_count,        /*  loop counter for communicator loop                */
     error,             /*  errors from one MPI call                          */
     fail,              /*  counts total number of failures                   */
     size,              /*  return size from MPI_Error_string                 */
     loop_cnt,          /*  counts total number of loops through test         */
     ierr,              /*  return value from MPI calls                       */
     max_length,        /*  maximum buffer length specified in config. file   */
     max_byte_length,   /*  maximum buffer length in bytes                    */
     root,              /*  the root of the current broadcast                 */
     source,            /*  source rank of where message is coming from       */
     tag,               /*  message tag                                       */
     maxnp,
     i, j;

#ifdef MPITEST_STATUS_CHK
   int count;           /*  length of message to be received from MPI_Get_count() */
#endif

   struct dataTemplate
     value;             /*  dataTemplate for initializing buffers             */

#if MPITEST_STATUS_CHK
   int  *buffer;      /* message buffer                                       */
#else
   int  buffer[2]     /* message buffer                                       */
#endif

   char
     info_buf[256],     /*  buffer for passing mesages to MPITEST             */
     testname[128];     /*  the name of this test                             */
    char error_string[MPI_MAX_ERROR_STRING];

   MPI_Comm comm;       /*  MPI communicator                                  */

   MPI_Request request; /*  Request handler                                   */
   MPI_Status status;

   int inter_flag;

   ierr = MPI_Init(&argc, &argv);
   if (ierr!=MPI_SUCCESS) {
      sprintf(info_buf, "MPI_Init() returned %d", ierr);
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

   sprintf(testname, "MPI_Probe_source");

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

      /* A measure to prevent having message flood the message buffer */
      /* when test scales up, all ranks larger than MPITEST_MAX_NP    */
      /* would NOT participate in message sending.  MPITEST_MAX_NP    */
      /* can be customized if needed at the top of this source.       */
      if (test_nump > MPITEST_MAX_NP) {
	 maxnp = MPITEST_MAX_NP;
      }
      else {
	 maxnp = test_nump;
      }

      if (test_nump < 2) {
	 /* Skipping communicator with comm size < 2 */
	 MPITEST_free_communicator(comm_type, &comm);
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
	    sprintf(info_buf, "Skipping inter communicator (commtype: %d) for this test", comm_type);
	    MPITEST_message(MPITEST_INFO1, info_buf);
	    continue;
	 }
	 
	 test_type = MPITEST_int;
	 
#ifdef MPITEST_STATUS_CHK
	 /* convert the number of bytes in the maximum length message */
	 /*  into the number of elements of the current type */
	 max_length = MPITEST_byte_to_element(test_type, max_byte_length);
	 
	 /* then allocate the buffer */
	 MPITEST_get_buffer(test_type, max_length, (void *) &buffer);
	 
	 for (length_count=0;length_count<MPITEST_num_message_lengths();length_count++) { 
	    byte_length = MPITEST_get_message_length(length_count);
	    length = MPITEST_byte_to_element(test_type, byte_length);
#else
	    length = 1; length_count = 0;
#endif
	    
	    for (root=0; root<maxnp; root++) {
	       /* print an informational message */
	       if (MPITEST_current_rank==0) {
		  sprintf(info_buf, "(%d, %d) length %d commsize %d commtype %d data_type %d root %d",
			  length_count, comm_count, length, test_nump, comm_type, test_type, root);
		  MPITEST_message(MPITEST_INFO1, info_buf);
	       }
	       
	       /* Set up the dataTemplate for initializing send buffer */
	       /* Initialize send buffer */
	       MPITEST_dataTemplate_init(&value, MPITEST_current_rank);
	       MPITEST_init_buffer(test_type, length+1,
				   value, buffer);

	       loop_cnt++;

	       tag = MPITEST_TAG;

	       if ((MPITEST_current_rank != root) && (MPITEST_current_rank < maxnp)) {
		  sprintf(info_buf, "Sending from source: %d to %d, tag: %d", MPITEST_current_rank, root, tag);
		  MPITEST_message(MPITEST_INFO1, info_buf);

		  ierr = MPI_Isend(buffer, length, MPITEST_mpi_datatypes[ test_type],
				  root, tag, comm, &request);
		  if (ierr != MPI_SUCCESS) {
		     fail++;
		     sprintf(info_buf, "MPI_Isend() returned %d", ierr);
		     MPITEST_message(MPITEST_NONFATAL, info_buf);
		     MPI_Error_string(ierr, error_string, &size);
		     MPITEST_message(MPITEST_FATAL, error_string);
		  }
	       }
	       else if (MPITEST_current_rank == root) {
		  /* Call MPI_Probe() with MPI_ANY_SOURCE first */

		  sprintf(info_buf, "Probing source: MPI_ANY_SOURCE (%d), tag: %d", MPI_ANY_SOURCE, tag);
		  MPITEST_message(MPITEST_INFO1, info_buf);

		  ierr = MPI_Probe(MPI_ANY_SOURCE, tag, comm, &status);
		  if (ierr != MPI_SUCCESS) {
		     fail++;
		     sprintf(info_buf, "MPI_Probe() returned %d, status.MPI_ERROR = %d",
			     ierr, status.MPI_ERROR);
		     MPITEST_message(MPITEST_NONFATAL, info_buf);
		     MPI_Error_string(ierr, error_string, &size);
		     MPITEST_message(MPITEST_FATAL, error_string);
		  }
		  else {
#ifdef MPITEST_STATUS_CHK
		     /* Check status.MPI_SOURCE */

		     sprintf(info_buf, "Verifying output status object");
		     MPITEST_message(MPITEST_INFO1, info_buf);

		     if (status.MPI_SOURCE < 0) {
			fail++;
			sprintf(info_buf,
				"status object returned from MPI_Probe() has negative MPI_SOURCE (%d) field",
				status.MPI_SOURCE);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
		     }
		     else if (status.MPI_SOURCE >= test_nump) {
			fail++;
			sprintf(info_buf,
				"status object returned from MPI_Probe() has too large MPI_SOURCE (%d) field",
				status.MPI_SOURCE);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
		     }

		     /* Check status.MPI_TAG */
		     if (status.MPI_TAG != tag) {
			fail++;
			sprintf(info_buf,
				"status object returned from MPI_Probe() has uncexpected MPI_TAG."
				"  Expected: %d, Actual: %d", tag, status.MPI_TAG);
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
			   sprintf(info_buf, "status object returned from MPI_Probe() contains unexpected length."
				   "  Expected: %d, Actual: %d", length, count);
			   MPITEST_message(MPITEST_NONFATAL, info_buf);
			}
		     }
#else
		     sprintf(info_buf, "output status object *not* verified");
		     MPITEST_message(MPITEST_INFO1, info_buf);
#endif            
		  }
		  /* No data to be received for MPI_ANY_SOURCE */

		  /* Call MPI_Probe() with other source ranks */
		  for (source = 0; source < maxnp; source++) {
		     if (source != root) {
			sprintf(info_buf, "Probing source: %d, tag: %d", source, tag);
			MPITEST_message(MPITEST_INFO1, info_buf);

			ierr = MPI_Probe(source, tag, comm, &status);
			if (ierr != MPI_SUCCESS) {
			   fail++;
			   sprintf(info_buf, "MPI_Probe() returned %d, status.MPI_ERROR = %d",
				   ierr, status.MPI_ERROR);
			   MPITEST_message(MPITEST_NONFATAL, info_buf);
			   MPI_Error_string(ierr, error_string, &size);
			   MPITEST_message(MPITEST_FATAL, error_string);
			}
			else {
#ifdef MPITEST_STATUS_CHK
			   /* Check status.MPI_SOURCE */
			   sprintf(info_buf, "Verifying output status object");
			   MPITEST_message(MPITEST_INFO1, info_buf);

			   if (status.MPI_SOURCE != source) {
			      fail++;
			      sprintf(info_buf,
				      "status object returned from MPI_Probe() has unexpected MPI_SOURCE field"
				      "Expected: %d, Actual: %d", source, status.MPI_SOURCE);
			      MPITEST_message(MPITEST_NONFATAL, info_buf);
			   }

			   /* Check status.MPI_TAG */
			   if (status.MPI_TAG != tag) {
			      fail++;
			      sprintf(info_buf,
				      "status object returned	from MPI_Probe() has uncexpected MPI_TAG."
				      "  Expected: %d, Actual: %d", tag, status.MPI_TAG);
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
				 sprintf(info_buf, "status object returned from MPI_Probe() contains unexpected length."
					 "  Expected: %d, Actual: %d", length, count);
				 MPITEST_message(MPITEST_NONFATAL, info_buf);
			      }
			   }
#else
			   sprintf(info_buf, "output status object *not* verified");
			   MPITEST_message(MPITEST_INFO1, info_buf);
#endif
			}
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

#ifdef MPITEST_BUFFER_RECV
	       if (MPITEST_current_rank == root) {
		  /* Call MPI_Probe() with other source ranks */
		  for (source = 0; source < maxnp; source++) {
		     if (source != root) {
			sprintf(info_buf, "Receiving from source: %d, tag: %d", source, tag);
			MPITEST_message(MPITEST_INFO1, info_buf);

			ierr = MPI_Recv(buffer, length, MPITEST_mpi_datatypes[ test_type],
					source, tag, comm, &status);
			if (ierr != MPI_SUCCESS) {
			   fail++;
			   sprintf(info_buf, "MPI_Recv() returned %d", ierr);
			   MPITEST_message(MPITEST_NONFATAL, info_buf);
			   MPI_Error_string(ierr, error_string, &size);
			   MPITEST_message(MPITEST_FATAL, error_string);
			}

#ifdef MPITEST_BUFFER_CHK
			sprintf(info_buf, "Verifying data received");
			MPITEST_message(MPITEST_INFO1, info_buf);
   
			/* Expecting to receive the rank number from each rank */
			MPITEST_dataTemplate_init(&value, source);
			error = MPITEST_buffer_errors(test_type, length, value, buffer);

			/* check for receive buffer overflow */
			MPITEST_dataTemplate_init(&value, MPITEST_current_rank);
			error += MPITEST_buffer_errors_ov(test_type,
							  length, value, buffer);

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
	       if ((MPITEST_current_rank != root) &&
		   (MPITEST_current_rank < maxnp)) {
		  /* Cancel message sent to root from each rank */
		  ierr = MPI_Wait(&request, &status);
		  if (ierr != MPI_SUCCESS) {
		     fail++;
		     sprintf(info_buf, "MPI_Wait() returned %d", ierr);
		     MPITEST_message(MPITEST_NONFATAL, info_buf);
		     MPI_Error_string(ierr, error_string, &size);
		     MPITEST_message(MPITEST_FATAL, error_string);
		  }
	       }
#else
	       if ((MPITEST_current_rank != root) &&
		   (MPITEST_current_rank < maxnp)) {
		  /* Cancel message sent to root from each rank */
		  sprintf(info_buf, "Cancelling message from source: %d, tag: %d", MPITEST_current_rank, tag);
		  MPITEST_message(MPITEST_INFO1, info_buf);

		  ierr = MPI_Cancel(&request);
		  if (ierr != MPI_SUCCESS) {
		     fail++;
		     sprintf(info_buf, "MPI_Cancel() returned %d", ierr);
		     MPITEST_message(MPITEST_NONFATAL, info_buf);
		     MPI_Error_string(ierr, error_string, &size);
		     MPITEST_message(MPITEST_FATAL, error_string);
		  }
		  ierr = MPI_Wait(&request, &status);
	       }
#endif
	       ierr = MPI_Barrier(comm);
	       if (ierr != MPI_SUCCESS) {
		  fail++;
		  sprintf(info_buf, "MPI_Barrier() returned %d", ierr);
		  MPITEST_message(MPITEST_NONFATAL, info_buf);
		  MPI_Error_string(ierr, error_string, &size);
		  MPITEST_message(MPITEST_FATAL, error_string);
	       }
	    }
#ifdef MPITEST_STATUS_CHK
	 }
#endif

         free(buffer);
      }

      MPITEST_free_communicator(comm_type, &comm);
   }
   /* report overall results  */
   MPITEST_report(loop_cnt-fail, fail, 0, testname);

   ierr = MPI_Finalize();
   if (ierr != MPI_SUCCESS) {
      fail++;
      sprintf(info_buf, "MPI_Finalize() returned %d, FAILED", ierr);
      MPITEST_message(MPITEST_NONFATAL, info_buf);
      MPI_Error_string(ierr, error_string, &size);
      MPITEST_message(MPITEST_FATAL, error_string);
   }

   return 0;
} /* main() */

