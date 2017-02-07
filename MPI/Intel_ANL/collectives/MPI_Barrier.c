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
                             Test for MPI_Barrier()

Thie code tests the functionality of the MPI_Barrier() function.  This
MPI function is supposed to cause all processes in a given
communicator to stop at a barrier until all others arrive at the same
barrier.

The idea of this test is for all ranks but one (which we will call the
root) to immediately enter the barrier, and when they exit the
barrier, they all send  a message to the root.  Before it enters the
barrier, the root probes for messages from the other ranks.  If
MPI_Barrier() is working properly, these probes (using the MPI
function MPI_Iprobe()) should not find any messages.  If a message is
found, then one of the non-root ranks must have passed the barrier.
This is an error since the root has not yet entered the barrier.

If the root process does not find any messages after a fixed number of
probes (the number of probes is fixed by the compiler constant
MAX_PROBE), then it enters the barrier.  After exiting the barrier,
the root process receives all the messages.

This test may be run in any type of communicator with any number of
processes participating.

The default behaviour is to loop over the communicator types and sizes
specified in include/mpitest_cfg.h .

******************************************************************************/

#include "mpitest_cfg.h"
#include "mpitest.h"

#define POST_BARRIER_TAG 999
#define MAX_PROBE 10000
#define PROBE_REPORT_PERIOD 1000

int main(int argc, char *argv[])
{
   int
      test_nump,		/* The number of processors in current
				 * communicator   */
      comm_index,		/* the array index of the current comm                */
      comm_type,		/* the index of the current communicator type         */
      comm_count,		/* loop counter for communicator loop                 */
      error,			/* errors from one MPI call                           */
      fail,			/* counts total number of failures                    */
      size,			/* return size from MPI_Error_string                 */
      loop_cnt,			/* counts total number of loops through test          */
      ierr,			/* return value from MPI calls                        */
      count,			/* number of items to send                            */
      flag,			/* did the Iprobe find a message ?                    */
      probe_count,		/* the current number of completed probes             */
      root,			/* the current checker for post-barrier
				 * messages      */
      i, j;

   void *buffer=0;		/* buffer for sending and receiving                   */

   char
      info_buf[256],		/* buffer for passing mesages to MPITEST              */
      testname[128];		/* the name of this test                              */
    char error_string[MPI_MAX_ERROR_STRING];


   MPI_Comm comm;		/* MPI communicator                                   */
   MPI_Status status;		/* status object                                      */

   int inter_flag;

   /* Initialize the MPI environment */
   ierr = MPI_Init(&argc, &argv);
   if (ierr != MPI_SUCCESS) {
      sprintf(info_buf, "MPI_Init() returned %d", ierr);
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

   sprintf(testname, "MPI_Barrier()");

   /* Initialize the MPITEST environment */
   MPITEST_init(argc, argv);

   if (MPITEST_me == 0) {
      sprintf(info_buf, "Starting %s test", testname);
      MPITEST_message(MPITEST_INFO0, info_buf);
   }

   /* set the global error counter */
   fail = 0;
   loop_cnt = 0;

   /* set the message length to zero */
   count = 0;


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
	 /* loop over different processes being the root */
	 for (root = 0; root < test_nump; root++) {
	 
	    /* print an informational message */
	    if (MPITEST_current_rank == 0) {
	       sprintf(info_buf, "(%d) commsize %d commtype %d root %d",
		       comm_count, test_nump, comm_type, root);
	       MPITEST_message(MPITEST_INFO1, info_buf);
	    }
	    
	    loop_cnt++;
	    /*
	     * the root rank probes, then enters the barrier, then receives
	     * messages
	     */
	    if (MPITEST_current_rank == root) {
	       flag = 0;
	       probe_count = 0;
	       while (!flag && (++probe_count) < MAX_PROBE) {
		  /* print out a progress report */
		  if (!(probe_count % PROBE_REPORT_PERIOD)) {
		     sprintf(info_buf, "Probe number %d", probe_count);
		     MPITEST_message(MPITEST_INFO1, info_buf);
		  }

		  /*
		   * look for post-barrier messages from any other process
		   */
		  ierr = MPI_Iprobe(MPI_ANY_SOURCE, POST_BARRIER_TAG,
				    comm, &flag, &status);
		  if (ierr != MPI_SUCCESS) {
		     sprintf(info_buf, "MPI_Iprobe() returned %d", ierr);
		     MPITEST_message(MPITEST_NONFATAL, testname);
		     MPI_Error_string(ierr, error_string, &size);
		     MPITEST_message(MPITEST_FATAL, error_string);
		     fail++;
		  }

	       }
	       /*
	        * If flag is true, then a post-barrier message was received
	        * pre-barrier, so the test fails
	        */
	       if (flag) {
		  fail++;
		  sprintf(info_buf, "Received a post-barrier msg from rank %d before entering barrier, (%d) commsize %d commtype %d root %d",
			  status.MPI_SOURCE,
			  comm_count, test_nump, comm_type, root);
		  MPITEST_message(MPITEST_NONFATAL, info_buf);
	       }
	       sprintf(info_buf, "Entering barrier");
	       MPITEST_message(MPITEST_INFO2, info_buf);

	       /* root process enters the barrier */
	       ierr = MPI_Barrier(comm);

	       if (ierr != MPI_SUCCESS) {
		  sprintf(info_buf, "MPI_Barrier() returned %d", ierr);
		  MPITEST_message(MPITEST_NONFATAL, info_buf);
		  MPI_Error_string(ierr, error_string, &size);
		  MPITEST_message(MPITEST_FATAL, error_string);
		  fail++;
	       }

	       /* root process receives messages */
	       for (i = 0; i < test_nump - 1; i++) {
		  ierr = MPI_Recv(buffer, count, MPI_INT, MPI_ANY_SOURCE,
				  POST_BARRIER_TAG + root, comm,
				  
				  
				  
				  &status);
		  if (ierr != MPI_SUCCESS) {
		     sprintf(info_buf, "MPI_Recv() returned %d", ierr);
		     MPITEST_message(MPITEST_NONFATAL, testname);
		     MPI_Error_string(ierr, error_string, &size);
		     MPITEST_message(MPITEST_FATAL, error_string);
		     fail++;
		  }

	       }
	       sprintf(info_buf, "Root received post-barrier messages");
	       MPITEST_message(MPITEST_INFO2, info_buf);
	    }
	    /* non-root processes enter barrier then send a message */
	    /* to root */
	    else {
			
			
	       sprintf(info_buf, "Entering barrier");
	       MPITEST_message(MPITEST_INFO2, info_buf);
	       
	       ierr = MPI_Barrier(comm);
	       
	       if (ierr != MPI_SUCCESS) {
		  sprintf(info_buf, "MPI_Barrier() returned %d", ierr);
		  MPITEST_message(MPITEST_NONFATAL, info_buf);
		  MPI_Error_string(ierr, error_string, &size);
		  MPITEST_message(MPITEST_FATAL, error_string);
		  fail++;
	       }
	       
	       ierr = MPI_Send(buffer, count, MPI_INT, root, POST_BARRIER_TAG + root,
			       comm);
	       
	       if (ierr != MPI_SUCCESS) {
		  sprintf(info_buf, "MPI_Send() returned %d", ierr);
		  MPITEST_message(MPITEST_NONFATAL, testname);
		  MPI_Error_string(ierr, error_string, &size);
		  MPITEST_message(MPITEST_FATAL, error_string);
		  fail++;
	       }

	    }
	 }

      }				/******** for (root=0;...) **********/

      MPITEST_free_communicator(comm_type, &comm);
   }				/********* for (comm_count=0;...) ************/

   /* report overall results  */
   MPITEST_report(loop_cnt - fail, fail, 0, testname);

   MPI_Finalize();
   return 0;
}				/* main() */
