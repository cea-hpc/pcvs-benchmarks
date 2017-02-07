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
                          Test for MPI_Cancel()

The test contains 2 phases:

1).
The root rank will submit Isend()s with different tags to the next rank
(circular).  The root rank will then do a MPI_Cancel() on request with even
tag and then followed by MPI_Wait() for the requests to be completed.  It then
call MPI_Test_cancelled() to verify that the selected send request has been
cancelled.  All participating ranks will then do a MPI_Barrier().

NOTE: This test assumes that an outstanding Send may be successfully
cancelled if an outstanding receive has not yet been posted.

2).
If things goes well with the first phase of the test, the receiving rank
will do a MPI_Recv() to receive the data.  The received data may or may not
be verified depending on whether MPITEST_BUFFER_CHK is #defined or not.
Depending on whether MPITEST_STATUS_CHK is #defined or not,
the status object returned by the MPI_Recv() may be checked.

This test may be run in any communicator with a minimum of 2 group members.

The MPITEST environment provides looping over communicator size, message
length, and root's rank.

The properties of the loops are encoded in configuration arrays in the
file mpitest_cfg.h .

MPI Calls dependencies for this test:
  MPI_Isend), MPI_Cancel(), MPI_Test_cancelled(), MPI_Recv(), MPI_Init(),
  MPI_Finalize(), MPI_Comm_test_inter(), MPI_Barrier(), MPI_Error_string(),
  MPI_Wait(), [MPI_Get_count(), MPI_Allreduce(), MPI_Comm_rank(), MPI_Comm_size()]
******************************************************************************/

#include "mpitest_cfg.h"
#include "mpitest.h"

/* The number of different transmissions in each innest loop */
/* WARNING : increasing over 2 may cause a send buffer to be re-used */
#define MPITEST_MAX_TAG       2

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
     tag,               /*  message tag                                       */
     maxtag,
     *attr_ub,
     found,
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

   MPI_Status status[MPITEST_MAX_TAG];

   MPI_Request send_request[MPITEST_MAX_TAG];

   int inter_flag, flag;

   ierr = MPI_Init(&argc, &argv);
   if (ierr!=MPI_SUCCESS) {
      sprintf(info_buf, "MPI_Init() returned %d", ierr);
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

   sprintf(testname, "MPI_Cancel_some");

   MPITEST_init(argc, argv);

   if (MPITEST_me==0) {
      sprintf(info_buf, "Starting %s test", testname);
      MPITEST_message(MPITEST_INFO0, info_buf);
   }

   /* set the global error counter */
   fail = 0;
   loop_cnt = 0;
   error = 0;

   /*  Get MPI_TAG_UB's value */
   ierr = MPI_Attr_get(MPI_COMM_WORLD, MPI_TAG_UB, &attr_ub, &found);
   if (ierr != MPI_SUCCESS) {
      sprintf(info_buf, "MPI_Attr_get() returned %d", ierr);
      MPITEST_message(MPITEST_NONFATAL, info_buf);
      MPI_Error_string(ierr, error_string, &size);
      MPITEST_message(MPITEST_FATAL, error_string);
   }

   if (!found) {
      sprintf(info_buf, "Cannot find a value for key MPI_TAG_UB");
      MPITEST_message(MPITEST_FATAL, info_buf);
   }
   else if (*attr_ub < MPITEST_TAG_UB_MIN) {
      sprintf(info_buf, "Attribute MPI_TAG_UB (%d) is less than the required minimum (%d)", *attr_ub, MPITEST_TAG_UB_MIN);
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

   if (MPITEST_MAX_TAG > *attr_ub) {
      maxtag = *attr_ub;
   }
   else {
      maxtag = MPITEST_MAX_TAG;
   }

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
	       /* Initialize buffer */
	       MPITEST_dataTemplate_init(&value, -1);

	       MPITEST_init_buffer(test_type, length+1,
					 value, buffer);

	       loop_cnt++;

	       /* Destination rank the message is being sent to */
	       dest = root + 1;

	       if (dest >= test_nump) {
		  dest = 0;
	       }

	       if (MPITEST_current_rank == root) {
		  for (tag = 0; tag < maxtag; tag++) {
		     sprintf(info_buf, "Sending message from source %d to %d, tag: %d", root, dest, tag);
		     MPITEST_message(MPITEST_INFO1, info_buf);

		     /* Initialize send buffer */
		     MPITEST_dataTemplate_init(&value, tag);

		     MPITEST_init_buffer(test_type, length+1,
					 value, buffer);

		     ierr = MPI_Isend(buffer, length, MPITEST_mpi_datatypes[test_type],
				      dest, tag, comm, &send_request[tag]);
		     if (ierr != MPI_SUCCESS) {
			fail++;
			sprintf(info_buf, "MPI_Isend() returned %d", ierr);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
			MPI_Error_string(ierr, error_string, &size);
			MPITEST_message(MPITEST_FATAL, error_string);
		     }

		     /* Cancelling some requests */
		     /* Canceling request with even tag */
		     if (tag % 2 == 0) {
			sprintf(info_buf, "Cancelling send request from source %d to %d, tag: %d", root, dest, tag);
			MPITEST_message(MPITEST_INFO1, info_buf);

			ierr = MPI_Cancel(&send_request[tag]);

			if (ierr != MPI_SUCCESS) {
			   fail++;
			   sprintf(info_buf, "MPI_Cancel() returned %d", ierr);
			   MPITEST_message(MPITEST_NONFATAL, info_buf);
			   MPI_Error_string(ierr, error_string, &size);
			   MPITEST_message(MPITEST_FATAL, error_string);
			}
			else {
			   sprintf(info_buf, "Waiting for send request to be cancelled");
			   MPITEST_message(MPITEST_INFO1, info_buf);

			   ierr = MPI_Wait(&send_request[tag], &status[tag]);

			   if (ierr != MPI_SUCCESS) {
			      fail++;
			      sprintf(info_buf, "MPI_Wait() returned %d", ierr);
			      MPITEST_message(MPITEST_NONFATAL, info_buf);
			      MPI_Error_string(ierr, error_string, &size);
			      MPITEST_message(MPITEST_FATAL, error_string);
			   }

			   sprintf(info_buf, "Checking if the send request has been cancelled");
			   MPITEST_message(MPITEST_INFO1, info_buf);

			   ierr = MPI_Test_cancelled(&status[tag], &flag);

			   if (ierr != MPI_SUCCESS) {
			      fail++;
			      sprintf(info_buf, "MPI_Test_cancelled() returned %d", ierr);
			      MPITEST_message(MPITEST_NONFATAL, info_buf);
			      MPI_Error_string(ierr, error_string, &size);
			      MPITEST_message(MPITEST_FATAL, error_string);			   
			   }
			   else if (!flag) {
			      fail++;
			      sprintf(info_buf, "MPI_Test_cancelled() return zero flag with a cancelled request");
			      MPITEST_message(MPITEST_NONFATAL, info_buf);
			   }
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


	       if (MPITEST_current_rank == dest) {
		  for (tag = maxtag - 1; tag >= 0; tag--) {
		     if (tag % 2 != 0) {
			sprintf(info_buf, "Receiving message from source %d to %d, tag: %d",
				root, dest, tag);
			MPITEST_message(MPITEST_INFO1, info_buf);

			ierr = MPI_Recv(buffer, length, MPITEST_mpi_datatypes[test_type],
					root, tag, comm, &status[tag]);
			if (ierr != MPI_SUCCESS) {
			   fail++;
			   sprintf(info_buf, "MPI_Recv() returned %d", ierr);
			   MPITEST_message(MPITEST_NONFATAL, info_buf);
			   MPI_Error_string(ierr, error_string, &size);
			   MPITEST_message(MPITEST_FATAL, error_string);
			}
			else {
#ifdef MPITEST_STATUS_CHK
			   /* Check status[tag].MPI_SOURCE */

			   sprintf(info_buf, "Verifying output status object");
			   MPITEST_message(MPITEST_INFO1, info_buf);

			   if (status[tag].MPI_SOURCE != root) {
			      fail++;
			      sprintf(info_buf,
				      "status object returned from MPI_Recv() has unexpected MPI_SOURCE (%d) field",
				      status[tag].MPI_SOURCE);
			      MPITEST_message(MPITEST_NONFATAL, info_buf);
			   }

			   /* Check status[tag].MPI_TAG */
			   if (status[tag].MPI_TAG != tag) {
			      fail++;
			      sprintf(info_buf,
				      "status object returned from MPI_Recv() has unexpected MPI_TAG"
				      "  Ecpected: %d, Actual: %d", tag, status[tag].MPI_TAG);
			      MPITEST_message(MPITEST_NONFATAL, info_buf);
			   }

			   /* Not checking status[tag].MPI_ERROR */

			   /* Check the length of the message to be received */
			   ierr = MPI_Get_count(&status[tag], MPITEST_mpi_datatypes[test_type], &count);
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
			   sprintf(info_buf, "output status object *not* verified");
			   MPITEST_message(MPITEST_INFO1, info_buf);
#endif		  
			}

#ifdef MPITEST_BUFFER_CHK
			/* Verifying buffer received */
			sprintf(info_buf, "Verifying data received");
			MPITEST_message(MPITEST_INFO1, info_buf);

			MPITEST_dataTemplate_init(&value, tag);
			error = MPITEST_buffer_errors(test_type, length, value, buffer);

			if (error) {
			   if (ierr == MPI_SUCCESS) fail++;
			   sprintf(info_buf, "%d errors in buffer (%d) len %d commsize %d commtype %d data_type %d root %d",
				   error, comm_count,
				   length, test_nump, comm_type, test_type, root);
			   MPITEST_message(MPITEST_NONFATAL, info_buf);
			}
			else {
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
	       if (MPITEST_current_rank == root) {
		  for (tag = 0; tag < maxtag; tag++) {
		     if (tag % 2 != 0) {
			   ierr = MPI_Wait(&send_request[tag], &status[tag]);

			   if (ierr != MPI_SUCCESS) {
			      fail++;
			      sprintf(info_buf, "MPI_Wait() returned %d", ierr);
			      MPITEST_message(MPITEST_NONFATAL, info_buf);
			      MPI_Error_string(ierr, error_string, &size);
			      MPITEST_message(MPITEST_FATAL, error_string);
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

