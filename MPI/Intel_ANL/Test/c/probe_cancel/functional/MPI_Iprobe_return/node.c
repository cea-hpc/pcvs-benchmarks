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

Test to make sure MPI_Iprobe() will return even if no message match the request.

The root rank will do a Iprobe() with MPI_ANY_TAG
followed by various tag used.  No message will be sent by any rank.
The result flag after the MPI_Iprobe() call will be checked to make sure it
is zero (false).

This test may be run in any communicator with a minimum of 2 group members.

The MPITEST environment provides looping over communicator size, root rank.
The properties of the loops are encoded in configuration arrays in the
file mpitest_cfg.h .

MPI Calls dependencies for this test:
  MPI_Iprobe(), MPI_Init(), MPI_Finalize()
  MPI_Comm_test_inter(), MPI_Barrier(), MPI_Error_string()
  MPI_Allreduce(), MPI_Comm_rank(), MPI_Comm_size()]
******************************************************************************/

#include "mpitest_cfg.h"
#include "mpitest.h"

#define MPITEST_MAX_TAG       5

int main(int argc, char *argv[])
{
   int
     test_type,         /*  the index of the current buffer type              */
     test_nump,         /*  The number of processors in current communicator  */
     comm_index,        /*  the array index of the current comm               */
     comm_type,         /*  the index of the current communicator type        */
     comm_count,        /*  loop counter for communicator loop                */
     error,             /*  errors from one MPI call                          */
     fail,              /*  counts total number of failures                   */
     size,              /*  return size from MPI_Error_string                 */
     loop_cnt,          /*  counts total number of loops through test         */
     ierr,              /*  return value from MPI calls                       */
     root,              /*  the root of the current broadcast                 */
     dest,              /*  destination rank of where message is being sent   */
     tag,               /*  message tag                                       */
     maxtag,
     maxnp,
     *attr_ub,
     found,
     i, j;

   char
     info_buf[256],     /*  buffer for passing mesages to MPITEST             */
     testname[128];     /*  the name of this test                             */
    char error_string[MPI_MAX_ERROR_STRING];

   MPI_Comm comm;       /*  MPI communicator                                  */

   MPI_Status status;

   int inter_flag, flag;

   ierr = MPI_Init(&argc, &argv);
   if (ierr!=MPI_SUCCESS) {
      sprintf(info_buf, "MPI_Init() returned %d", ierr);
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

   sprintf(testname, "MPI_Iprobe_return");

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

   for (comm_count=0; comm_count<MPITEST_num_comm_sizes();comm_count++) {
      comm_index = MPITEST_get_comm_index(comm_count);
      comm_type = MPITEST_get_comm_type(comm_count);

      test_nump = MPITEST_get_communicator(comm_type, comm_index, &comm);

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
	    continue;
	 }
	 
	 test_type = MPITEST_int;
	 
	 for (root=0; root<test_nump; root++) {
	    /* print an informational message */
	    if (MPITEST_current_rank==0) {
	       sprintf(info_buf, "(%d) commsize %d commtype %d root %d",
		       comm_count, test_nump, comm_type, root);
	       MPITEST_message(MPITEST_INFO1, info_buf);
	    }
	    
	    loop_cnt++;

	    if (MPITEST_current_rank == root) {
	       /* Call MPI_Iprobe() with MPI_ANY_TAG first */

	       sprintf(info_buf, "Probing source: %d, tag: MPI_ANY_TAG (%d)", root, MPI_ANY_TAG);
	       MPITEST_message(MPITEST_INFO1, info_buf);

	       flag = 1;
	       ierr = MPI_Iprobe(root, MPI_ANY_TAG, comm, &flag, &status);
	       if (ierr != MPI_SUCCESS) {
		  fail++;
		  sprintf(info_buf, "MPI_Iprobe() returned %d", ierr);
		  MPITEST_message(MPITEST_NONFATAL, info_buf);
		  MPI_Error_string(ierr, error_string, &size);
		  MPITEST_message(MPITEST_FATAL, error_string);
	       }
	       else if (flag) {
		  sprintf(info_buf, "MPI_Iprobe() return flag = TRUE when no message has been posted");
		  MPITEST_message(MPITEST_FATAL, info_buf);
	       }

	       for (tag = maxtag - 1; tag >= 0; tag--) {
		  sprintf(info_buf, "Probing source: %d, tag: %d", root, tag);
		  MPITEST_message(MPITEST_INFO1, info_buf);

		  flag = 1;
		  ierr = MPI_Iprobe(root, tag, comm, &flag, &status);
		  if (ierr != MPI_SUCCESS) {
		     fail++;
		     sprintf(info_buf, "MPI_Iprobe() returned %d", ierr);
		     MPITEST_message(MPITEST_NONFATAL, info_buf);
		     MPI_Error_string(ierr, error_string, &size);
		     MPITEST_message(MPITEST_FATAL, error_string);
		  }
		  else if (flag) {
		     sprintf(info_buf, "MPI_Iprobe() return flag = TRUE when no message has been posted");
		     MPITEST_message(MPITEST_FATAL, info_buf);
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
      MPITEST_free_communicator(comm_type, &comm);
   }
   /* report overall results  */
   MPITEST_report(loop_cnt-fail, fail, 0, testname);

   MPI_Finalize();
   return 0;
} /* main() */

