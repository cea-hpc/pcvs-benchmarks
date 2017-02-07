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
                          Test for MPI_Type_hindexed()

All rank will call MPI_Type_hindexed() and then MPI_Type_commit() to create
a new user defined type using basic type defined in the types array in mpitest_cfg.h.
The root rank will then send a message using the new type to the receiving rank.
The receiveing rank will do a recv.  The received buffer will be verified.
Depending on whether MPI_TEST_STATUS_CHK is #defined or not, the output status
object from MPI_Recv() may ot may not be verified. 

The input count to MPI_Type_hindexed() is determined by each message length
in mpitest_cfg.h and the input oldtype to MPI_Type_hindexed() is determined
by the input types in mpitest_cfg.h.

This test may be run in any communicator with a minimum of 2 group members,
with any data type, and with any non-negative message length.

The MPITEST environment provides looping over communicator size,
message length and the root's rank.  The properties of the loops are
encoded in configuration arrays in the file mpitest_cfg.h .

MPI Calls dependencies for this test:
  MPI_Send(), MPI_Recv(), MPI_Cancel(), MPI_Init(), MPI_Finalize()
  MPI_Comm_test_inter(), MPI_Error_string(), MPI_Type_extent(),
  MPI_Type_hindexed(), MPI_Type_commit(), MPI_Type_free(),
  [MPI_Get_count(), MPI_Allreduce(), MPI_Comm_rank(), MPI_Comm_size()]

Test history:
   1  07/17/96     simont       Original version

******************************************************************************/

#include "mpitest_cfg.h"
#include "mpitest.h"

/* Tag to be used for message transmission in this test            */
#define MPITEST_TAG           1

/* Additional buffer size */
#define MPITEST_ADD_BUFSIZE   20

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
     type_count,        /*  loop counter for data type loop                   */
     comm_count,        /*  loop counter for communicator loop                */
     error,             /*  errors from one MPI call                          */
     fail,              /*  counts total number of failures                   */
     size,              /*  return size from MPI_Error_string                 */
     loop_cnt,          /*  counts total number of loops through test         */
     ierr,              /*  return value from MPI calls                       */
     max_length,        /*  maximum buffer length specified in config. file   */
     max_byte_length,   /*  maximum buffer length in bytes                    */
     root,              /*  the root of the current broadcast                 */
     tag,               /*  message tag                                       */
     dest;              /*  Destination rank                                  */


#ifdef MPITEST_STATUS_CHK
   int count,           /*  length of message to be received from MPI_Get_count() */
       expected_count;
#endif

   struct dataTemplate
     value;             /*  dataTemplate for initializing buffers             */

   int  *buffer;      /* message buffer                                       */

   int *blklens, i;

   MPI_Aint *displs, extent;

   char
     info_buf[256],     /*  buffer for passing mesages to MPITEST             */
     testname[128];     /*  the name of this test                             */
    char error_string[MPI_MAX_ERROR_STRING];

   MPI_Comm comm;       /*  MPI communicator                                  */

   MPI_Status status;

   MPI_Datatype newtype;/*  User defined datatype to be created               */

   int inter_flag;

   ierr = MPI_Init(&argc, &argv);
   if (ierr!=MPI_SUCCESS) {
      sprintf(info_buf, "MPI_Init() returned %d", ierr);
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

   sprintf(testname, "MPI_Type_hindexed_basic");

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

      if (comm != MPI_COMM_NULL) {
	 if (test_nump < 2) {
	    /* Skipping communicator with comm size < 2 */
	    MPITEST_free_communicator(comm_type, &comm);
	    sprintf(info_buf, "Skipping communicator with comm_size < 2 (commtype: %d) for this test", comm_type);
	    MPITEST_message(MPITEST_INFO1, info_buf);
	    continue;
	 }

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
	 
	 for (type_count=0; type_count < MPITEST_num_datatypes(); type_count++) {
	    test_type = MPITEST_get_datatype(type_count);
	    
	    if (test_type >= MPITEST_derived1) {
	       sprintf(info_buf, "Skipping non-basic datatype");
	       MPITEST_message(MPITEST_INFO1, info_buf);

	       continue;
	    }

	    ierr = MPI_Type_extent(MPITEST_mpi_datatypes[test_type], &extent);
	    if (ierr != MPI_SUCCESS) {
	       fail++;
	       sprintf(info_buf, "MPI_Type_extent() returned %d", ierr);
	       MPITEST_message(MPITEST_NONFATAL, info_buf);
	       MPI_Error_string(ierr, error_string, &size);
	       MPITEST_message(MPITEST_FATAL, error_string);
	    }
	       
	    /* convert the number of bytes in the maximum length message */
	    /*  into the number of elements of the current type */
	    max_length = MPITEST_byte_to_element(test_type, max_byte_length);

	    /* So there will always be at least one element of the newly */
	    /* created type to be use in data transmission */
	    if (max_length < extent) max_length = extent;

	    /* then allocate the buffer */
	    MPITEST_get_buffer(test_type, max_length + MPITEST_ADD_BUFSIZE, (void *) &buffer);
	    
	    for (length_count=0;length_count<MPITEST_num_message_lengths();length_count++) { 
	       byte_length = MPITEST_get_message_length(length_count);
	       
	       length = byte_length / extent;

	       /* So, there will be at least one element of the newly */
	       /* created type to be used in data transmission */
	       if (length == 0) {
		  sprintf(info_buf, "element length = 0, using length = 1");
		  MPITEST_message(MPITEST_INFO1, info_buf);

		  length = 1;
	       }

	       displs = (MPI_Aint *) calloc(length, sizeof(MPI_Aint));
	       if (!displs) {
		  sprintf(info_buf, "Cannot allocate enough space for displs array");
		  MPITEST_message(MPITEST_FATAL, info_buf);
	       }
	       
	       blklens = (int *) calloc(length, sizeof(int));
	       if (!blklens) {
		  sprintf(info_buf, "Cannot allocate enough space for blklens array");
		  MPITEST_message(MPITEST_FATAL, info_buf);
	       }
	       
	       for (i = 0; i < length; i++) {
		  blklens[i] = 1;
		  displs[i] = i * extent;
	       }

	       /* Create new user defined datatype */
	       ierr = MPI_Type_hindexed(length, blklens, displs, MPITEST_mpi_datatypes[test_type], &newtype);
	       if (ierr != MPI_SUCCESS) {
		  fail++;
		  sprintf(info_buf, "MPI_Type_hindexed() returned %d", ierr);
		  MPITEST_message(MPITEST_NONFATAL, info_buf);
		  MPI_Error_string(ierr, error_string, &size);
		  MPITEST_message(MPITEST_FATAL, error_string);
	       }
	       
	       /* Committing newly created datatype */
	       ierr = MPI_Type_commit(&newtype);
	       if (ierr != MPI_SUCCESS) {
		  fail++;
		  sprintf(info_buf, "MPI_Type_commit() returned %d", ierr);
		  MPITEST_message(MPITEST_NONFATAL, info_buf);
		  MPI_Error_string(ierr, error_string, &size);
		  MPITEST_message(MPITEST_FATAL, error_string);
	       }
	       
	       /* Now test the newly create type in data tranmission */
	       for (root=0; root<test_nump; root++) {
		  /* print an informational message */
		  if (MPITEST_current_rank==0) {
		     sprintf(info_buf, "(%d, %d) length %d commsize %d commtype %d data_type %d root %d",
			     length_count, comm_count, length, test_nump, comm_type, test_type, root);
		     MPITEST_message(MPITEST_INFO1, info_buf);
		  }
		  
		  /*  Set up the dataTemplate for initializing buffer */
		  /* Initialize buffer */
		  MPITEST_dataTemplate_init(&value, MPITEST_current_rank);
		  MPITEST_init_buffer(test_type, length+1,
				      value, buffer);

		  loop_cnt++;

		  tag = MPITEST_TAG;

		  /* Destination rank */
		  dest = root + 1;
		  if (dest >= test_nump) {
		     dest = 0;
		  }

		  if (MPITEST_current_rank == root) {
		     sprintf(info_buf, "Sending from source: %d to %d, tag: %d", root, dest, tag);
		     MPITEST_message(MPITEST_INFO1, info_buf);

		     ierr = MPI_Send(buffer, 1, newtype,
				     dest, tag, comm);
		     if (ierr != MPI_SUCCESS) {
			fail++;
			sprintf(info_buf, "MPI_Send() returned %d", ierr);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
			MPI_Error_string(ierr, error_string, &size);
			MPITEST_message(MPITEST_FATAL, error_string);
		     }
		  }
		  else if (MPITEST_current_rank == dest) {
		     /* Receive data from root's rank */
		     sprintf(info_buf, "Receiving from source: %d to %d, tag: %d", root, dest, tag);
		     MPITEST_message(MPITEST_INFO1, info_buf);

		     ierr = MPI_Recv(buffer, 1, newtype,
				  root, tag, comm, &status);
		     if (ierr != MPI_SUCCESS) {
			fail++;
			sprintf(info_buf, "MPI_Recv() returned %d",
				ierr);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
			MPI_Error_string(ierr, error_string, &size);
			MPITEST_message(MPITEST_FATAL, error_string);
		     }
		     else {
#ifdef MPITEST_STATUS_CHK
			/* Check status.MPI_SOURCE */
			sprintf(info_buf, "Verifying output status object");
			MPITEST_message(MPITEST_INFO1, info_buf);

			if (status.MPI_SOURCE != root) {
			   fail++;
			   sprintf(info_buf,
				   "status object returned from MPI_Recv() has unexpected MPI_SOURCE field"
				   "Expected: %d, Actual: %d", root, status.MPI_SOURCE);
			   MPITEST_message(MPITEST_NONFATAL, info_buf);
			}

			/* Check status.MPI_TAG */
			if (status.MPI_TAG != tag) {
			   fail++;
			   sprintf(info_buf,
				   "status object returned	from MPI_Recv() has uncexpected MPI_TAG."
				   "  Expected: %d, Actual: %d", tag, status.MPI_TAG);
			   MPITEST_message(MPITEST_NONFATAL, info_buf);
			}

			/* Not checking status.MPI_ERROR */

			/* Check the length of the message received */
			if (length != 0) {
			   expected_count = 1;
			}
			else {
			   expected_count = 0;
			}

			ierr = MPI_Get_count(&status, newtype, &count);
			if (ierr != MPI_SUCCESS) {
			   fail++;
			   sprintf(info_buf, "MPI_Get_count() returned %d", ierr);
			   MPITEST_message(MPITEST_NONFATAL, info_buf);
			   MPI_Error_string(ierr, error_string, &size);
			   MPITEST_message(MPITEST_FATAL, error_string);
			}
			else {
			   if (count != expected_count) {
			      fail++;
			      sprintf(info_buf, "status object returned from MPI_Recv() contains unexpected length."
				      "  Expected: %d, Actual: %d", expected_count, count);
			      MPITEST_message(MPITEST_NONFATAL, info_buf);
			   }
			}
#else
			sprintf(info_buf, "output status object *not* verified");
			MPITEST_message(MPITEST_INFO1, info_buf);
#endif
		     }

		     /* Verify the received buffer */
		     sprintf(info_buf, "Verifying data received");
		     MPITEST_message(MPITEST_INFO1, info_buf);

		     /* Expecting to receive the root rank number */
		     MPITEST_dataTemplate_init(&value, root);
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
		  }
	       }

	       /* free newly created datatype */
	       ierr = MPI_Type_free(&newtype);
	       if (ierr != MPI_SUCCESS) {
		  fail++;
		  sprintf(info_buf, "MPI_Type_free() returned %d", ierr);
		  MPITEST_message(MPITEST_NONFATAL, info_buf);
		  MPI_Error_string(ierr, error_string, &size);
		  MPITEST_message(MPITEST_FATAL, error_string);
	       }

	       free(blklens);
	       free(displs);
	    }

	    free(buffer);
	 }
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

