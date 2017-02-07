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
                          Test for MPI_Type_free()

Test to verify that derived datatype from a freed datatype will not be
affected in a MPI_Type_free().

All ranks will first do a MPI_Type_struct() of all types in the type config
array (in mpitest_cfg.h).  Using this new datatype, Another datatype will also
be created.  The first datatype will then be freed using MPI_Type_free().

Root's rank will then do a Isend() using the 2nd user datatype.
The destination rank will then receive and verify the data using the user
datatype.

This test may be run in any communicator with a minimum of 2 group members,
with any data type, and with any non-negative message length.

The MPITEST environment provides looping over communicator size,
message length and the root's rank.  The properties of the loops are
encoded in configuration arrays in the file mpitest_cfg.h .

MPI Calls dependencies for this test:
  MPI_Send(), MPI_Recv(), MPI_Init(), MPI_Finalize()
  MPI_Comm_test_inter(), MPI_Error_string(),
  MPI_Type_struct(), MPI_Type_commit(), MPI_Barrier(),
  MPI_Type_size(), MPI_Type_extent(), MPI_Type_free(),
  [MPI_Get_count(), MPI_Allreduce(), MPI_Comm_rank(), MPI_Comm_size()]

Test history:
   1  07/09/96     simont       Original version

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
     byte_length,       /*  The byte length of data to be sent / recv         */
     test_nump,         /*  The number of processors in current communicator  */
     comm_index,        /*  the array index of the current comm               */
     comm_type,         /*  the index of the current communicator type        */
     type_count,        /*  loop counter for data type loop                   */
     comm_count,        /*  loop counter for communicator loop                */
     error,             /*  error for data received                           */
     error2,            /*  error for data received                           */
     fail,              /*  counts total number of failures                   */
     size,              /*  return size from MPI_Error_string                 */
     loop_cnt,          /*  counts total number of loops through test         */
     ierr,              /*  return value from MPI calls                       */
     max_length,        /*  maximum buffer length specified in config. file   */
     root,              /*  the root of the current broadcast                 */
     tag,               /*  message tag                                       */
     dest;              /*  Destination rank                                  */

   int i, j, k;         /*  Loop counter for for loop in verifying received   */
                        /*  data                                              */

   int acc;             /*  for accumulating type sizes within a block        */

   int pos;             /*  index for checking received data buffer           */

#ifdef MPITEST_STATUS_CHK
   int count;           /*  length of message to be received from MPI_Get_count() */
#endif

   signed char *buffer; /*  message buffer (test assumes signed)              */

   char
     info_buf[256],     /*  buffer for passing mesages to MPITEST             */
     testname[128];     /*  the name of this test                             */
    char error_string[MPI_MAX_ERROR_STRING];
   MPI_Comm comm;       /*  MPI communicator                                  */

   MPI_Status status;

   MPI_Datatype newtype,
                newtype2,
                *types,
                types2[1];

   int *blklens, blklens2[1], num_types, *type_sizes, basic_types,
       type_sizes2[1];

   MPI_Aint *displs, displs2[1], extent;

   int inter_flag;

   ierr = MPI_Init(&argc, &argv);
   if (ierr!=MPI_SUCCESS) {
      sprintf(info_buf, "MPI_Init() returned %d", ierr);
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

   sprintf(testname, "MPI_Type_free_pending_msg");

   MPITEST_init(argc, argv);

   if (MPITEST_me==0) {
      sprintf(info_buf, "Starting %s test", testname);
      MPITEST_message(MPITEST_INFO0, info_buf);
   }

   /* set the global error counter */
   fail = 0;
   loop_cnt = 0;

   /* find the maximum length we will need */
   max_length = MPITEST_get_max_message_length();

   num_types = MPITEST_num_datatypes();

   /* Set up various arrays */
   types = (MPI_Datatype *) calloc(num_types, sizeof(MPI_Datatype));
   if (!types) {
      sprintf(info_buf, "Cannot allocate enough memory for types array");
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

   blklens = (int *) calloc(num_types, sizeof(int));
   if (!blklens) {
      sprintf(info_buf, "Cannot allocate enough memory for blklens array");
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

   type_sizes = (int *) calloc(num_types, sizeof(int));
   if (!type_sizes) {
      sprintf(info_buf, "Cannot allocate enough memory for type_sizes array");
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

   displs = (MPI_Aint *) calloc(num_types, sizeof(MPI_Aint));
   if (!displs) {
      sprintf(info_buf, "Cannot allocate enough memory for displs array");
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

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
	 
	 basic_types = 0;
	 for (type_count = 0; type_count < num_types; type_count++) {
	    test_type = MPITEST_get_datatype(type_count);
	    /* Only use basic type */
            if (test_type < MPITEST_derived1) {
	       types[basic_types] = MPITEST_mpi_datatypes[test_type];

	       ierr = MPI_Type_extent(types[basic_types], &extent);
	       if (ierr != MPI_SUCCESS) {
		  fail++;
		  sprintf(info_buf, "MPI_Type_extent() returned %d", ierr);
		  MPITEST_message(MPITEST_NONFATAL, info_buf);
		  MPI_Error_string(ierr, error_string, &size);
		  MPITEST_message(MPITEST_FATAL, error_string);
	       }

	       if (basic_types == 0) {
	          displs[basic_types] = 0;
	       }
	       else {
	          displs[basic_types] = displs[basic_types - 1] +
		     type_sizes[basic_types - 1];
	       }

	       blklens[basic_types] = 1;

	       ierr = MPI_Type_size(types[basic_types], &(type_sizes[basic_types]));
	       if (ierr != MPI_SUCCESS) {
	          sprintf(info_buf, "MPI_Type_size() returned %d", ierr);
	          MPITEST_message(MPITEST_NONFATAL, info_buf);
	          MPI_Error_string(ierr, error_string, &size);
	          MPITEST_message(MPITEST_FATAL, error_string);
	       }
	       basic_types++;
	    }
	 }

	 if (basic_types == 0)
	    MPITEST_message(MPITEST_FATAL, "No basic data types configured");

	 if (MPITEST_current_rank == 0) {
	    for (type_count = 0; type_count < basic_types; type_count++) {
	       sprintf(info_buf, "blklens[%d] = %d, displs[%d] = %d, "
		       "types[%d] = %d, type_sizes[%d] = %d",
		       type_count, blklens[type_count],
		       type_count, displs[type_count],
		       type_count, types[type_count],
		       type_count, type_sizes[type_count]);
	       MPITEST_message(MPITEST_INFO2, info_buf);
	    }
	 }
	 
	 for (length_count=0;length_count<MPITEST_num_message_lengths();length_count++) { 
	    byte_length = MPITEST_get_message_length(length_count);
	    
	    /* Now test the newly create type in data tranmission */
	    for (root=0; root<test_nump; root++) {
	       ierr = MPI_Type_struct(basic_types, blklens, displs, types, &newtype);
	       if (ierr != MPI_SUCCESS) {
		  sprintf(info_buf, "MPI_Type_struct() returned %d", ierr);
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

	       ierr = MPI_Type_extent(newtype, &extent);
	       if (ierr != MPI_SUCCESS) {
		  sprintf(info_buf, "MPI_Type_extent() returned %d", ierr);
		  MPITEST_message(MPITEST_NONFATAL, info_buf);
		  MPI_Error_string(ierr, error_string, &size);
		  MPITEST_message(MPITEST_FATAL, error_string);
	       }

	       length = byte_length / extent;

	       /* So, there will be at least one element of the newly */
	       /* created type to be used in data transmission */
	       if (length == 0) {
		  sprintf(info_buf, "element length = 0, using length = 1");
		  MPITEST_message(MPITEST_INFO1, info_buf);

		  length = 1;
	       }

	       /* print an informational message */
	       if (MPITEST_current_rank==0) {
		  sprintf(info_buf, "(%d, %d) length %d commsize %d commtype %d extent %d root %d",
			  length_count, comm_count, length, test_nump, comm_type, extent, root);
		  MPITEST_message(MPITEST_INFO1, info_buf);
	       }

	       /* Create another datatype base on this type */
	       types2[0] = newtype;
	       displs2[0] = 0;
	       blklens2[0] = 1;

	       ierr = MPI_Type_struct(1, blklens2, displs2, types2, &newtype2);
	       if (ierr != MPI_SUCCESS) {
		  sprintf(info_buf, "MPI_Type_struct() returned %d", ierr);
		  MPITEST_message(MPITEST_NONFATAL, info_buf);
		  MPI_Error_string(ierr, error_string, &size);
		  MPITEST_message(MPITEST_FATAL, error_string);
	       }

	       /* Committing newly created datatype */
	       ierr = MPI_Type_commit(&newtype2);
	       if (ierr != MPI_SUCCESS) {
		  fail++;
		  sprintf(info_buf, "MPI_Type_commit() returned %d", ierr);
		  MPITEST_message(MPITEST_NONFATAL, info_buf);
		  MPI_Error_string(ierr, error_string, &size);
		  MPITEST_message(MPITEST_FATAL, error_string);
	       }

	       /* So there will always be at least one element of the newly */
	       /* created type to be use in data transmission */
	       if (max_length < extent) max_length = extent;

	       /* then allocate the buffer */
	       /* Allocate more than we need so if receiver receive more than */
	       /* sipposed to, there will still be enough space for the program */
	       /* to continue */
	       MPITEST_get_buffer(MPITEST_byte, max_length + MPITEST_ADD_BUFSIZE, (void *) &buffer);

	       /* Initialize send & recv buffer */
	       if (MPITEST_current_rank != root) {
		  for (i = 0; i < max_length + MPITEST_ADD_BUFSIZE; i++) {
		     buffer[i] = -1 * MPITEST_current_rank;
		  }
	       }
	       else {
		  /* Root's rank */
		  for (i = 0; i < length; i++) {
		     /* Initialize each block */
		     acc = 0;
		     for (j = 0; j < basic_types; j++) {
			/* Initialize each basic type */
			for (k = 0; k < type_sizes[j]; k++) {
			   /* Use type_count + 1 as send data */
			   pos = i * extent + acc + k;

			   buffer[pos] = j + 1;
			}
			acc += type_sizes[j];
		     }
		  }

		  /* Also initialize the unused data buffer as well */
		  for (i = length * extent; i < max_length + MPITEST_ADD_BUFSIZE; i++) {
		     buffer[i] = -1 * MPITEST_current_rank;
		  }
	       }

	       loop_cnt++;

	       tag = MPITEST_TAG;

	       /* Destination rank */
	       dest = root + 1;
	       if (dest >= test_nump) {
		  dest = 0;
	       }

	       sprintf(info_buf, "Freeing user defined type 1");
	       MPITEST_message(MPITEST_INFO1, info_buf);

	       ierr = MPI_Type_free(&newtype);
	       if (ierr != MPI_SUCCESS) {
		  fail++;
		  sprintf(info_buf, "MPI_Type_free() returned %d", ierr);
		  MPITEST_message(MPITEST_NONFATAL, info_buf);
		  MPI_Error_string(ierr, error_string, &size);
		  MPITEST_message(MPITEST_FATAL, error_string);
	       }
	       else if (newtype != MPI_DATATYPE_NULL) {
		  fail++;
		  sprintf(info_buf, "type is not set to MPI_DATATYPE_NULL by MPI_Type_free()");
		  MPITEST_message(MPITEST_NONFATAL, info_buf);
	       }

	       if (MPITEST_current_rank == root) {
		  sprintf(info_buf, "Sending from source: %d to %d, tag: %d", root, dest, tag);
		  MPITEST_message(MPITEST_INFO1, info_buf);

#ifdef MPITEST_DISP_BUF
		     sprintf(info_buf, "Displaying Sender's data buffer:");
		     MPITEST_message(MPITEST_INFO2, info_buf);

		     for (i = 0; i < max_length + MPITEST_ADD_BUFSIZE; i++) {
			sprintf(info_buf, "buffer[%d] = %d", i, buffer[i]);
			MPITEST_message(MPITEST_INFO2, info_buf);
		     }
#endif
		  ierr = MPI_Send(buffer, length, newtype2,
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

		  ierr = MPI_Recv(buffer, length, newtype2,
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

		     ierr = MPI_Get_count(&status, newtype2, &count);
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

		  /* Verify the received buffer */
		  sprintf(info_buf, "Verifying data received");
		  MPITEST_message(MPITEST_INFO1, info_buf);

		  error = 0;
		  error2 = 0;

		  for (i = 0; i < length; i++) {
		     /* Verify each block */
		     acc = 0;
		     for (j = 0; j < basic_types; j++) {
			/* Verify each basic type */
			for (k = 0; k < type_sizes[j]; k++) {
			   pos = i * extent + acc + k;

			   if (buffer[pos] != j + 1) {
			      fail++;
			      error++;
#ifdef MPITEST_1ST_ERR
			      if (error <= 1) {
#endif
				 sprintf(info_buf,
					 "Unexpected received buffer[%d], Expected: %d, Actual: %d",
					 pos, j + 1, buffer[pos]);
				 MPITEST_message(MPITEST_NONFATAL, info_buf);
#ifdef MPITEST_1ST_ERR
			      }
#endif
			   }
			}
			acc += type_sizes[j];
		     }
		  }

		  /* Verify that receiver did not receive more data than expected */
		  if (buffer[length * extent] != -1 * MPITEST_current_rank) {
		     fail++;
		     error2++;
#ifdef MPITEST_1ST_ERR
		     if (error2 <= 1) {
#endif
			sprintf(info_buf,
				"Received buffer overflow, Expected buffer[%d]: %d,"
				" Actual buffer[%d]: %d",
				length * extent, -1 * MPITEST_current_rank,
				length * extent, buffer[length * extent]);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
#ifdef MPITEST_1ST_ERR
		     }
#endif
		  }

		  if (error + error2) {
		     if (ierr == MPI_SUCCESS) fail++;

#ifdef MPITEST_DISP_BUF
		     sprintf(info_buf, "Displaying received data buffer:");
		     MPITEST_message(MPITEST_INFO2, info_buf);

		     for (i = 0; i < max_length + MPITEST_ADD_BUFSIZE; i++) {
			sprintf(info_buf, "buffer[%d] = %d", i, buffer[i]);
			MPITEST_message(MPITEST_INFO2, info_buf);
		     }
#endif
		     sprintf(info_buf, "%d errors in buffer (%d, %d) len %d commsize %d commtype %d extent %d root %d",
			     error + error2, length_count, comm_count, length, test_nump, comm_type, extent, root);
		     MPITEST_message(MPITEST_NONFATAL, info_buf);
		  } else {
		     sprintf(info_buf,"0 errors found in buffer");
		     MPITEST_message(MPITEST_INFO2, info_buf);
		  }

	       }

	       /* Free newly created datatype */
	       sprintf(info_buf, "Freeing user defined type 2");
	       MPITEST_message(MPITEST_INFO1, info_buf);

	       ierr = MPI_Type_free(&newtype2);
	       if (ierr != MPI_SUCCESS) {
		  fail++;
		  sprintf(info_buf, "MPI_Type_free() returned %d", ierr);
		  MPITEST_message(MPITEST_NONFATAL, info_buf);
		  MPI_Error_string(ierr, error_string, &size);
		  MPITEST_message(MPITEST_FATAL, error_string);
	       }
	       else if (newtype != MPI_DATATYPE_NULL) {
		  fail++;
		  sprintf(info_buf, "type is not set to MPI_DATATYPE_NULL by MPI_Type_free()");
		  MPITEST_message(MPITEST_NONFATAL, info_buf);
	       }

	       free(buffer);

#ifdef MPITEST_SYNC
	       ierr = MPI_Barrier(comm);
	       if (ierr != MPI_SUCCESS) {
		  sprintf(info_buf, "MPI_Barrier() returned %d", ierr);
		  MPITEST_message(MPITEST_NONFATAL, info_buf);
		  MPI_Error_string(ierr, error_string, &size);
		  MPITEST_message(MPITEST_FATAL, error_string);
	       }
#endif
	    }
	 }
      }
      MPITEST_free_communicator(comm_type, &comm);
   }
   free(types);
   free(blklens);
   free(displs);

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

