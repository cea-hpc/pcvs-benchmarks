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
                          Test for MPI_Pack()

All ranks will first do a MPI_Type_struct() of all types in the type config
array (in mpitest_cfg.h).  The root's rank will then pack the message and send
it to the next rank (circular).  The receiver rank will then receive the
message and then unpack it.  The input type to MPI_Pack() and MPI_Unpack()
is the newly contruct user datatype.  The output unpacked message data will be
verified and depending on whether MPITEST_STATUS_CHK #defined or not,
the output status object may or may not be verified.

MPITEST_DISPL defined the displacement space between any 2 basic type input
from mpitest_cfg.h

This test may be run in any communicator with a minimum of 2 group members,
with any data type, and with any non-negative message length.

The MPITEST environment provides looping over communicator size,
message length and the root's rank.  The properties of the loops are
encoded in configuration arrays in the file mpitest_cfg.h .

MPI Calls dependencies for this test:
  MPI_Send(), MPI_Recv(), MPI_Init(), MPI_Finalize(),
  MPI_Comm_test_inter(), MPI_Error_string(), MPI_Pack(),
  MPI_Type_struct(), MPI_Type_commit(), MPI_Pack_size(),
  MPI_Type_size(), MPI_Type_extent(), MPI_Type_free(),
  [MPI_Get_count(), MPI_Allreduce(), MPI_Comm_rank(), MPI_Comm_size()]

Test history:
   1  07/18/96     simont       Original version

******************************************************************************/

#include "mpitest_cfg.h"
#include "mpitest.h"

/* Tag to be used for message transmission in this test            */
#define MPITEST_TAG           1

/* Additional buffer size */
#define MPITEST_ADD_BUFSIZE   20

/* Addition displacement (in bytes) between each type */
#define MPITEST_DISPL         10

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
     max_length,        /*  maximum buffer length specified in mpitest_cfg.h  */
     root,              /*  the root of the current broadcast                 */
     tag,               /*  message tag                                       */
     position,          /*  position where buffer starts                      */
     dest;              /*  Destination rank                                  */

   int i, j, k;         /*  Loop counter for for loop in verifying received   */
                        /*  data                                              */

   int pos;             /*  index for checking received data buffer           */

#ifdef MPITEST_STATUS_CHK
   int count;           /*  length of message to be received from MPI_Get_count() */
#endif

   int pack_size;       /*  Size of pack message                              */

   signed char *in_buffer, *packed_buffer, *buf_ptr;  
                        /* message buffer (test assumed signed)               */

   char
     info_buf[256],     /*  buffer for passing mesages to MPITEST             */
     testname[128];     /*  the name of this test                             */
    char error_string[MPI_MAX_ERROR_STRING];

   MPI_Comm comm;       /*  MPI communicator                                  */

   MPI_Status status;

   MPI_Datatype newtype,
                *types;

   int *blklens, num_types, *type_sizes, basic_types;

   MPI_Aint *displs, extent;

   int inter_flag;

   ierr = MPI_Init(&argc, &argv);
   if (ierr!=MPI_SUCCESS) {
      sprintf(info_buf, "MPI_Init() returned %d", ierr);
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

   sprintf(testname, "MPI_Pack_displs");

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
		     type_sizes[basic_types - 1] + MPITEST_DISPL;
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

	 if (MPITEST_current_rank == 0) {
	    for (type_count = 0; type_count < basic_types; type_count++) {
	       sprintf(info_buf, "blklens[%d] = %d, displs[%d] = "
		       MPITEST_AINT_FMT_DEC_SPEC ", types[%d] = %d, type_sizes[%d] = %d",
		       type_count, blklens[type_count],
		       type_count, displs[type_count],
		       type_count, types[type_count],
		       type_count, type_sizes[type_count]);
	       MPITEST_message(MPITEST_INFO2, info_buf);
	    }
	 }

	 /* So there will always be at least one element of the newly */
	 /* created type to be use in data transmission */
         if (max_length < extent) max_length = extent;

	 /* Allocate more than we need so if receiver receive more than */
	 /* supposed to, there will still be enough space for the program */
	 /* to continue */
	 MPITEST_get_buffer(MPITEST_byte, max_length + MPITEST_ADD_BUFSIZE, (void *) &in_buffer);

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
	    
	    /* Now test the newly create type in data tranmission */
	    for (root=0; root<test_nump; root++) {
	       /* print an informational message */
	       if (MPITEST_current_rank==0) {
		  sprintf(info_buf, "(%d, %d) length %d commsize %d commtype %d extent " MPITEST_AINT_FMT_DEC_SPEC " root %d",
			  length_count, comm_count, length, test_nump, comm_type, extent, root);
		  MPITEST_message(MPITEST_INFO1, info_buf);
	       }

	       /* Initialize send & recv buffer */
	       for (i = 0; i < max_length + MPITEST_ADD_BUFSIZE; i++) {
		  in_buffer[i] = -1 * MPITEST_current_rank;
	       }

	       if (MPITEST_current_rank == root) {
		  /* Root's rank */

		  for (i = 0; i < length; i++) {
		     /* Initialize each block */
		     for (j = 0; j < basic_types; j++) {
			/* Initialize each basic type */
			for (k = 0; k < type_sizes[j]; k++) {
			   /* Use type_count + 1 as send data */
			   pos = i * extent + displs[j] + k;

			   in_buffer[pos] = j + 1;
			}
		     }
		  }

		  /* Also initialize the unused data buffer as well */
		  for (i = length * extent; i < max_length + MPITEST_ADD_BUFSIZE; i++) {
		     in_buffer[i] = -1 * MPITEST_current_rank;
		  }
	       }

	       loop_cnt++;

	       tag = MPITEST_TAG;

	       /* Destination rank */
	       dest = root + 1;
	       if (dest >= test_nump) {
		  dest = 0;
	       }

	       sprintf(info_buf, "Calculating space needed for packing message...");
	       MPITEST_message(MPITEST_INFO1, info_buf);

	       ierr = MPI_Pack_size(length, newtype, comm, &pack_size);
	       if (ierr != MPI_SUCCESS) {
		  fail++;
		  sprintf(info_buf, "MPI_Pack_size() returned %d", ierr);
		  MPITEST_message(MPITEST_NONFATAL, info_buf);
		  MPI_Error_string(ierr, error_string, &size);
		  MPITEST_message(MPITEST_FATAL, error_string);
	       }

	       sprintf(info_buf, "Packing message size is %d", pack_size);
	       MPITEST_message(MPITEST_INFO1, info_buf);

	       MPITEST_get_buffer(MPITEST_byte, pack_size + MPITEST_ADD_BUFSIZE, (void *) &packed_buffer);
	       
	       if (MPITEST_current_rank == root) {
		  sprintf(info_buf, "Packing message to be sent...");
		  MPITEST_message(MPITEST_INFO1, info_buf);

		  position = 0;

		  buf_ptr = in_buffer;

		  ierr = MPI_Pack(buf_ptr, length, newtype,
				  packed_buffer, pack_size + MPITEST_ADD_BUFSIZE,
				  &position, comm);
		  if (ierr != MPI_SUCCESS) {
		     fail++;
		     sprintf(info_buf, "MPI_Pack() returned %d", ierr);
		     MPITEST_message(MPITEST_NONFATAL, info_buf);
		     MPI_Error_string(ierr, error_string, &size);
		     MPITEST_message(MPITEST_FATAL, error_string);
		  }

#ifdef MPITEST_DISP_BUF
		  sprintf(info_buf, "Displaying UNPACKED Sender's data buffer:");
		  MPITEST_message(MPITEST_INFO2, info_buf);

		  for (i = 0; i < max_length + MPITEST_ADD_BUFSIZE; i++) {
		     sprintf(info_buf, "buffer[%d] = %d", i, in_buffer[i]);
		     MPITEST_message(MPITEST_INFO2, info_buf);
		  }
#endif
		  /* Sending packed message size to receiver */

		  sprintf(info_buf, "Sending message size from source: %d to %d, tag: %d", root, dest, tag + 1);
		  MPITEST_message(MPITEST_INFO1, info_buf);

		  ierr = MPI_Send(&position, 1, MPI_INT,
				  dest, tag + 1, comm);
		  if (ierr != MPI_SUCCESS) {
		     fail++;
		     sprintf(info_buf, "MPI_Send() returned %d", ierr);
		     MPITEST_message(MPITEST_NONFATAL, info_buf);
		     MPI_Error_string(ierr, error_string, &size);
		     MPITEST_message(MPITEST_FATAL, error_string);
		  }

		  sprintf(info_buf, "Sending PACKED data from source: %d to %d, tag: %d", root, dest, tag);
		  MPITEST_message(MPITEST_INFO1, info_buf);

		  ierr = MPI_Send(packed_buffer, position, MPI_PACKED,
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
		  /* Receive packed message size */
		  sprintf(info_buf, "Receiving message size from source: %d to %d, tag: %d", root, dest, tag + 1);
		  MPITEST_message(MPITEST_INFO1, info_buf);

		  ierr = MPI_Recv(&position, 1, MPI_INT, root, tag + 1,
				  comm, &status);
		  if (ierr != MPI_SUCCESS) {
		     fail++;
		     sprintf(info_buf, "MPI_Recv() returned %d",
			     ierr);
		     MPITEST_message(MPITEST_NONFATAL, info_buf);
		     MPI_Error_string(ierr, error_string, &size);
		     MPITEST_message(MPITEST_FATAL, error_string);
		  }

		  /* Receive data from root's rank */
		  sprintf(info_buf, "Receiving PACKED data from source: %d to %d, tag: %d", root, dest, tag);
		  MPITEST_message(MPITEST_INFO1, info_buf);

		  ierr = MPI_Recv(packed_buffer, position, MPI_PACKED,
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

		     ierr = MPI_Get_count(&status, MPI_PACKED, &count);
		     if (ierr != MPI_SUCCESS) {
			fail++;
			sprintf(info_buf, "MPI_Get_count() returned %d", ierr);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
			MPI_Error_string(ierr, error_string, &size);
			MPITEST_message(MPITEST_FATAL, error_string);
		     }
		     else {
			if (count != position) {
			   fail++;
			   sprintf(info_buf, "status object returned from MPI_Recv() contains unexpected length."
				   "  Expected: %d, Actual: %d", position, count);
			   MPITEST_message(MPITEST_NONFATAL, info_buf);
			}
		     }
#else
		     sprintf(info_buf, "output status object *not* verified");
		     MPITEST_message(MPITEST_INFO1, info_buf);
#endif
		  }

		  /* Unpacking buffer */
		  sprintf(info_buf, "Unpacking message received...");
		  MPITEST_message(MPITEST_INFO1, info_buf);

		  position = 0;

		  buf_ptr = in_buffer;

		  ierr = MPI_Unpack(packed_buffer, pack_size + MPITEST_ADD_BUFSIZE,
				    &position, buf_ptr, length, newtype, comm);
		  if (ierr != MPI_SUCCESS) {
		     fail++;
		     sprintf(info_buf, "MPI_Unpack() returned %d", ierr);
		     MPITEST_message(MPITEST_NONFATAL, info_buf);
		     MPI_Error_string(ierr, error_string, &size);
		     MPITEST_message(MPITEST_FATAL, error_string);
		  }

		  /* Verify the received buffer */
		  sprintf(info_buf, "Verifying data received");
		  MPITEST_message(MPITEST_INFO1, info_buf);

		  error = 0;
		  error2 = 0;

		  for (i = 0; i < length; i++) {
		     /* Verify each block */
		     for (j = 0; j < basic_types; j++) {
			/* Verify each basic type */
			for (k = 0; k < type_sizes[j]; k++) {
			   pos = i * extent + displs[j] + k;

			   if (in_buffer[pos] != j + 1) {
			      fail++;
			      error++;
#ifdef MPITEST_1ST_ERR
			      if (error <= 1) {
#endif
				 sprintf(info_buf,
					 "Unexpected received buffer[%d], Expected: %d, Actual: %d",
					 pos, j + 1, in_buffer[pos]);
				 MPITEST_message(MPITEST_NONFATAL, info_buf);
#ifdef MPITEST_1ST_ERR
			      }
#endif
			   }
			}
		     }
		  }

		  /* Verify that receiver did not receive more data than expected */
		  if (in_buffer[length * extent] != -1 * MPITEST_current_rank) {
		     fail++;
		     error2++;
#ifdef MPITEST_1ST_ERR
		     if (error2 <= 1) {
#endif
			sprintf(info_buf,
				"Received buffer overflow, Expected buffer[" MPITEST_AINT_FMT_DEC_SPEC "]: %d,"
				" Actual buffer[" MPITEST_AINT_FMT_DEC_SPEC "]: %d",
				length * extent, -1 * MPITEST_current_rank,
				length * extent, in_buffer[length * extent]);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
#ifdef MPITEST_1ST_ERR
		     }
#endif
		  }

		  if (error + error2) {
		     if (ierr == MPI_SUCCESS) fail++;

#ifdef MPITEST_DISP_BUF
		     sprintf(info_buf, "Displaying UNPACKED received data buffer:");
		     MPITEST_message(MPITEST_INFO2, info_buf);

		     for (i = 0; i < max_length + MPITEST_ADD_BUFSIZE; i++) {
			sprintf(info_buf, "buffer[%d] = %d", i, in_buffer[i]);
			MPITEST_message(MPITEST_INFO2, info_buf);
		     }
#endif
		     sprintf(info_buf, "%d errors in buffer (%d, %d) len %d commsize %d commtype %d extent " MPITEST_AINT_FMT_DEC_SPEC " root %d",
			     error + error2, length_count, comm_count, length, test_nump, comm_type, extent, root);
		     MPITEST_message(MPITEST_NONFATAL, info_buf);
		  } else {
		     sprintf(info_buf,"0 errors found in buffer");
		     MPITEST_message(MPITEST_INFO2, info_buf);
		  }

	       }
	       free(packed_buffer);

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

	 /* Free newly created datatype */
	 ierr = MPI_Type_free(&newtype);
	 if (ierr != MPI_SUCCESS) {
	    fail++;
	    sprintf(info_buf, "MPI_Type_free() returned %d", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_FATAL, error_string);
	 }

	 free(in_buffer);
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

