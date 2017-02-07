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
                          Test for MPI_Iprobe()

The root rank will submit Isend() of various tag to all other ranks.
Each rank (except the root rank) will do a Iprobe() with MPI_ANY_TAG
followed by various tag used.  Depending on whether MPITEST_BUFFER_RECV
and or MPITEST_BUFFER_CHK is / are defined or not, a corresponding recv()
may be issued and the recv buffer may be checked.

This test may be run in any communicator with a minimum of 2 group members.
The test only sends 1 MPI_INT in each request.

The MPITEST environment provides looping over communicator size.
The properties of the loops are encoded in configuration arrays in the
file mpitest_cfg.h .

MPI Calls dependencies for this test:
  MPI_Isend(), MPI_Irecv(), MPI_Cancel(), MPI_Iprobe(), MPI_Init(), MPI_Finalize()
  MPI_Comm_test_inter(), MPI_Barrier(), MPI_Error_string()
  [MPI_Get_count(), MPI_Allreduce(), MPI_Comm_rank(), MPI_Comm_size()]
******************************************************************************/

#include "mpitest_cfg.h"
#include "mpitest.h"

#define MPITEST_MAX_TAG       5
#define MPITEST_MAX_NP       10

/* Comment the following #define out to turn off receiving buffer */
#define MPITEST_BUFFER_RECV   1

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
     maxnp,
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

   void *buffer[MPITEST_MAX_RANKS][MPITEST_MAX_TAG]; /* message buffer                          */

   char
     info_buf[256],     /*  buffer for passing mesages to MPITEST             */
     testname[128];     /*  the name of this test                             */
    char error_string[MPI_MAX_ERROR_STRING];

   MPI_Comm comm;       /*  MPI communicator                                  */

   MPI_Status status;

   MPI_Request *recv_request, send_request[100][MPITEST_MAX_TAG];

   int inter_flag, flag;

   ierr = MPI_Init(&argc, &argv);
   if (ierr!=MPI_SUCCESS) {
      sprintf(info_buf, "MPI_Init() returned %d", ierr);
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

   sprintf(testname, "MPI_Iprobe_tag");

   MPITEST_init(argc, argv);

   if (MPITEST_me==0) {
      sprintf(info_buf, "Starting %s test", testname);
      MPITEST_message(MPITEST_INFO0, info_buf);
   }

   /* set the global error counter */
   fail = 0;
   loop_cnt = 0;

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
	    sprintf(info_buf, "Skipping inter communicator (commtype: %d) for this test", comm_type);
	    continue;
	 }
	 
	 /* Allocate request arrays */
	 recv_request = (MPI_Request *) malloc(maxtag * sizeof(MPI_Request));
	 if (recv_request == NULL) {
	    sprintf(info_buf, "Unable to allocate enough space for receive requests array, FAILED");
	    MPITEST_message(MPITEST_FATAL, info_buf);
	 }
	 
	 test_type = MPITEST_int;
	 
	 /* convert the number of bytes in the maximum length message */
	 /*  into the number of elements of the current type */
	 max_length = (MPITEST_byte_to_element(test_type, max_byte_length)
		+ test_nump) / test_nump;
	 
	 /* then allocate the buffer */
	 for (j = 0; j < test_nump; j++) {
	    for (i = 0; i < maxtag; i++) {
	       MPITEST_get_buffer(test_type, max_length, &buffer[j][i]);
	    }
	 }
	 
#ifdef MPITEST_STATUS_CHK
	 for (length_count=0;length_count<MPITEST_num_message_lengths();length_count++) {
	    byte_length = MPITEST_get_message_length(length_count);
	    length = MPITEST_byte_to_element(test_type, byte_length);
	    if (length > max_length) length = max_length;
#else
	    length = 1; length_count = 0;
#endif
	    
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

	       for (j = 0; j < test_nump; j ++) {
		  for (i = 0; i < maxtag; i++) {
		     MPITEST_init_buffer(test_type, length+1,
					 value, buffer[j][i]);
		  }
	       }

	       loop_cnt++;

	       if (MPITEST_current_rank != root) {
		  /* Call MPI_Iprobe() with MPI_ANY_TAG first */

		  sprintf(info_buf, "Probing source: %d, tag: MPI_ANY_TAG (%d)", root, MPI_ANY_TAG);
		  MPITEST_message(MPITEST_INFO1, info_buf);

		  do {
		     ierr = MPI_Iprobe(root, MPI_ANY_TAG, comm, &flag, &status);
		     if (ierr != MPI_SUCCESS) {
			fail++;
			sprintf(info_buf, "MPI_Iprobe() returned %d", ierr);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
			MPI_Error_string(ierr, error_string, &size);
			MPITEST_message(MPITEST_FATAL, error_string);
		     }
		  } while (!flag);

#ifdef MPITEST_STATUS_CHK
		  /* Check status.MPI_SOURCE */

		  sprintf(info_buf, "Verifying output status object");
		  MPITEST_message(MPITEST_INFO1, info_buf);

		  if (status.MPI_SOURCE != root) {
		     fail++;
		     sprintf(info_buf,
			     "status object returned from MPI_Iprobe() has unexpected MPI_SOURCE (%d) field",
			     status.MPI_SOURCE);
		     MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }

		  /* Check status.MPI_TAG */
		  if (status.MPI_TAG < 0) {
		     fail++;
		     sprintf(info_buf,
			     "status object returned	from MPI_Iprobe() has negative MPI_TAG (%d).",
			     status.MPI_TAG);
		     MPITEST_message(MPITEST_NONFATAL, info_buf);
		  }
		  else if (status.MPI_TAG >= maxtag) {
		     fail++;
		     sprintf(info_buf,
			     "status object returned	from MPI_Iprobe() has too large MPI_TAG (%d).",
			     status.MPI_TAG);
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
			sprintf(info_buf, "status object returned from MPI_Iprobe() contains unexpected length."
				"  Expected: %d, Actual: %d", length, count);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
		     }
		  }
#else
		  sprintf(info_buf, "output status object *not* verified");
		  MPITEST_message(MPITEST_INFO1, info_buf);
#endif            
		  /* No data to be received for MPI_ANY_TAG */

		  for (tag = maxtag - 1; tag >= 0; tag--) {
		     sprintf(info_buf, "Probing source: %d, tag: %d", root, tag);
		     MPITEST_message(MPITEST_INFO1, info_buf);

		     do {
			ierr = MPI_Iprobe(root, tag, comm, &flag, &status);
			if (ierr != MPI_SUCCESS) {
			   fail++;
			   sprintf(info_buf, "MPI_Iprobe() returned %d", ierr);
			   MPITEST_message(MPITEST_NONFATAL, info_buf);
			   MPI_Error_string(ierr, error_string, &size);
			   MPITEST_message(MPITEST_FATAL, error_string);
			}
		     } while (!flag);

#ifdef MPITEST_STATUS_CHK
		     /* Check status.MPI_SOURCE */
		     sprintf(info_buf, "Verifying output status object");
		     MPITEST_message(MPITEST_INFO1, info_buf);

		     if (status.MPI_SOURCE != root) {
			fail++;
			sprintf(info_buf,
				"status object returned from MPI_Iprobe() has unexpected MPI_SOURCE field"
				"Expected: %d, Actual: %d", root, status.MPI_SOURCE);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
		     }

		     /* Check status.MPI_TAG */
		     if (status.MPI_TAG != tag) {
			fail++;
			sprintf(info_buf,
				"status object returned	from MPI_Iprobe() has uncexpected MPI_TAG."
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
			   sprintf(info_buf, "status object returned from MPI_Iprobe() contains unexpected length."
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
	       else {
		  /* Send message to all ranks in communicator except myself (root) */
		  for (dest = 0; dest < test_nump; dest++) {
		     if (dest != root) {
			for (tag = 0; tag < maxtag; tag++) {
			   ierr = MPI_Isend(buffer[dest][tag], length, MPITEST_mpi_datatypes[test_type],
					    dest, tag, comm, &(send_request[dest][tag]));

			   if (ierr != MPI_SUCCESS) {
			      fail++;
			      sprintf(info_buf, "MPI_Isend() returned %d", ierr);
			      MPITEST_message(MPITEST_NONFATAL, info_buf);
			      MPI_Error_string(ierr, error_string, &size);
			      MPITEST_message(MPITEST_FATAL, error_string);
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

#ifdef MPITEST_BUFFER_RECV
	       if (MPITEST_current_rank != root) {
		  for (tag = maxtag - 1; tag >= 0; tag--) {
		     sprintf(info_buf, "Receiving source: %d, tag: %d", root, tag);
		     MPITEST_message(MPITEST_INFO1, info_buf);

		     ierr = MPI_Irecv(buffer[0][0], length, MPITEST_mpi_datatypes[test_type],
				      root, tag, comm, &(recv_request[tag]));
		     if (ierr != MPI_SUCCESS) {
			fail++;
			sprintf(info_buf, "MPI_Irecv() returned %d", ierr);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
			MPI_Error_string(ierr, error_string, &size);
			MPITEST_message(MPITEST_FATAL, error_string);
		     }

		     MPI_Wait(&(recv_request[tag]), &status);

#ifdef MPITEST_BUFFER_CHK
		     /* Expecting to receive the sender's rank number */
		     MPITEST_dataTemplate_init(&value, root);
		     error = MPITEST_buffer_errors(test_type, length, value, buffer[0][0]);

		     /* check for receive buffer overflow */
		     MPITEST_dataTemplate_init(&value, MPITEST_current_rank);
		     error += MPITEST_buffer_errors_ov(test_type,
					  length, value, buffer[0][0]);

		     if (error) {
			if (ierr == MPI_SUCCESS) fail++;
			sprintf(info_buf, "%d errors in buffer (%d) len %d commsize %d commtype %d data_type %d root %d",
				error, comm_count,
				length, test_nump, comm_type, test_type, root);
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
	       if (MPITEST_current_rank == root) {
		  /* Wait on messages sent to all ranks in communicator */
		  for (dest = 0; dest < test_nump; dest++) {
		     if (dest != root) {
			for (tag = 0; tag < maxtag; tag++) {
			   ierr = MPI_Wait(&(send_request[dest][tag]), &status);

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
	       }
#else
	       if (MPITEST_current_rank == root) {
		  /* Cancel message sent to all ranks in communicator */
		  for (dest = 0; dest < test_nump; dest++) {
		     if (dest != root) {
			for (tag = 0; tag < maxtag; tag++) {
			   ierr = MPI_Cancel(&(send_request[dest][tag]));

			   if (ierr != MPI_SUCCESS) {
			      fail++;
			      sprintf(info_buf, "MPI_Cancel() returned %d", ierr);
			      MPITEST_message(MPITEST_NONFATAL, info_buf);
			      MPI_Error_string(ierr, error_string, &size);
			      MPITEST_message(MPITEST_FATAL, error_string);
			   }
			   ierr = MPI_Wait(&(send_request[dest][tag]), &status);
			}
		     }
		  }
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
	    } /* root */
#ifdef MPITEST_STATUS_CHK
	 } /* length */
#endif
      for (j = 0; j < test_nump; j++) {
        for (i = 0; i < maxtag; i++) {
           free(buffer[j][i]);
        }
      }

      free(recv_request);
      
      }


      MPITEST_free_communicator(comm_type, &comm);
   }
   /* report overall results  */
   MPITEST_report(loop_cnt-fail, fail, 0, testname);

   MPI_Finalize();
   return 0;
} /* main() */

