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
                          MPI_Dims_create

 Reference:  MPI Standard,  Chapter 6:  Process Topologies
                            Section 6.5.2

 Uses only intra-communicators,  all inter-communicators are skipped
 Must have a minimum of MINNODE nodes in the communicator
 The test program is limited to testing not more than MAXNDIMS dimensions

                         periods = TRUE for all dimensions
                         reorder = TRUE

******************************************************************************/
#define  MAXNDIMS    4	  /* Maximum dimensions to generate                  */
#define  MINNODE     6	  /* Minimum number of nodes on which test will run  */

#include "mpitest_cfg.h"
#include "mpitest.h"

int main(int argc, char *argv[])
{
   int
      i, j,		/* General loop indices                              */
      comm_count,	/* loop counter for communicator loop                */
      comm_index,       /* the array index of the current comm               */
      comm_type,	/* the index of the current communicator type        */
      error,		/* errors from one MPI call                          */
      errorclass,	/* error class of ierr                               */
      fail = 0,		/* counts total number of failures                   */
      loop_fail,	/* counts number of failures in loop                 */
      ierr,		/* return value from MPI calls                       */
      inter_comm,	/* Intercommunicator flag, true if intercommunicator */
      ndims,		/* Number of dimensions to generate coordinates for  */
      nnodes,		/* node count for Dims_create                        */
      reorder,		/* rank reorder flag                                 */
      size,		/* return size from MPI_Error_string                 */
      test_nump,        /* The number of processors in current communicator  */
      world_nump,	/* number of processes in MPI_COMM_WORLD             */
      test_val,		/* communication test value                          */
      rank_world,	/* rank in MPI_COMM_WORLD                            */
      sum,		/* sum of Cartesian ranks for checking communicator  */
      tot_dim_size,	/* product of dimensions                             */
      zval,		/* utility variable for testing generated dimensions */
      zloc,		/* utility variable for testing generated dimensions */
      new_val,		/* utility variable for testing generated dimensions */
      nzeros,		/* number of zeros in generated dimensions           */
      loop_cnt;		/* test event count                                  */

   int
      orig_dim_size[MAXNDIMS],   /* Assigned dimension sizes                 */
      dim_size[MAXNDIMS];        /* Cartesian dimension sizes                */

   char
      info_buf[256],	/* buffer for passing mesages to MPITEST             */
      testname[64];	/* the name of this test                             */
    char error_string[MPI_MAX_ERROR_STRING];

   MPI_Comm
      comm;		/* MPITEST communicator                              */

   /*------------------------------  MPI_Init  -----------------------------*/
   ierr = MPI_Init(&argc, &argv);
   if (ierr != MPI_SUCCESS) {
      sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr);
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

   sprintf(testname, "MPI_Dims_create:  Mixed Dimension Assignments");

   /*-----------------------------  MPITEST_init  --------------------------*/
   MPITEST_init(argc, argv);

   if (MPITEST_me == 0) {
      sprintf(info_buf, "Starting %s test", testname);
      MPITEST_message(MPITEST_INFO0, info_buf);
   }

   MPI_Comm_size(MPI_COMM_WORLD, &world_nump);
   MPI_Comm_rank(MPI_COMM_WORLD, &rank_world);

   if (world_nump < MINNODE) {
	 fail++;
	 sprintf(info_buf, "ERROR --  nodes = %3d   Need %3d nodes to run test", world_nump, MINNODE);
	 MPITEST_message(MPITEST_FATAL, info_buf);
   }

   /* set the global error counter */
   fail = 0;
   loop_cnt = 0;

   /*----------------------  Loop over Communicators  ----------------------*/

   for (comm_count = 0; comm_count < MPITEST_num_comm_sizes(); comm_count++) {
      comm_index = MPITEST_get_comm_index(comm_count);
      comm_type = MPITEST_get_comm_type(comm_count);
      test_nump = MPITEST_get_communicator(comm_type, comm_index, &comm);

      if (MPITEST_current_rank != MPI_UNDEFINED) {
	 /* ...  inter_comm is true if  inter-communicator ... */

	 MPI_Comm_test_inter(comm, &inter_comm);

	 /* Decide how many dimensions to generate  */

	 ndims = MAXNDIMS;

	 while (test_nump % ndims != 0)
	    ndims--;

	 switch (ndims) {
	 case 1:
	    dim_size[0] = 0;
	    break;
	 case 2:
	    dim_size[0] = test_nump / 2;
	    dim_size[1] = 0;
	    break;
	 case 3:
	    dim_size[0] = test_nump / 3;
	    dim_size[1] = 0;
	    dim_size[2] = 0;
	    break;
	 case 4:
	    dim_size[0] = test_nump / 4;
	    if (test_nump % 16 == 0)
	       dim_size[0] = test_nump / 8;
	    dim_size[1] = 0;
	    dim_size[2] = 0;
	    dim_size[3] = 0;
	    if (test_nump % 12 == 0) {
	       dim_size[0] = 0;
	       dim_size[1] = 3;
	    }
	    break;
	 default:
	    fail++;
	    sprintf(info_buf, "Error in dimension calculation:  ndims = %d", ndims);
	    MPITEST_message(MPITEST_FATAL, info_buf);
	 }
	 for (i = 0; i < ndims; i++) {
	    orig_dim_size[i] = dim_size[i];
	 }
	 /*-----------------------  MPI_Dims_create  ----------------------*/

	 loop_cnt++;
	 nnodes = test_nump;
	 ierr = MPI_Dims_create(nnodes,
				ndims,
				dim_size);

	 if (ierr != MPI_SUCCESS) {
	    fail++;
	    MPI_Error_class(ierr, &errorclass);
	    sprintf(info_buf, "MPI_Dims_create error (%d)  Errorclass = %d", ierr, errorclass);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_NONFATAL, error_string);
	 }

	 /* Check dims_size array for zeros  */

	 nzeros = 0;
	 for (i = 0; i < ndims; i++) {
	    if (dim_size[i] < 1)
	       nzeros++;
	 }
	 if (MPITEST_current_rank == 0) {
	    if (nzeros) {
	       sprintf(info_buf, "Dimension sizes < 1 after call to MPI_Dims_create  %d bad values", nzeros);
	       MPITEST_message(MPITEST_NONFATAL, info_buf);
	       fail++;
	    }
	 }

	 /* Verify that nnodes = product of dimensions  */
	 tot_dim_size = 1;
	 for (i = 0; i < ndims; i++)
	    tot_dim_size *= dim_size[i];

	 if (tot_dim_size != nnodes) {
	    sprintf(info_buf, "Nnodes does not equal product of dimension sizes:  Nnodes = %d   tot_dim_size = %d", nnodes, tot_dim_size);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    fail++;
	 }

	 /*
	  * Verify that elements of the dim_size array set to positive
	  * integers on input are not changed
	  */
	 error = 0;
	 for (i = 0; i < ndims; i++) {
	    if (orig_dim_size[i] > 0 && orig_dim_size[i] != dim_size[i]) {
	       error++;
	    }
	 }
	 if (error) {
	    sprintf(info_buf, "User assigned dimensions changed by MPI_Dims_create in  %d  cases", error);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    fail++;
	 }

	 /*
	  * Verify that for dim_size[i] set by Dims_create ( dim_size[i]
	  * initialized to zero) the values assigned are in non-increasing
	  * order
	  */

	 /* First, see if there were any zeros in the input dimensions  */
	 error = 0;
	 i = 0;
	 while (orig_dim_size[i] != 0 && i < ndims)
	    i++;
	 zloc = i;
	 if (zloc < ndims) {
	    /* There are zeros in the original dimensions  */
	    zval = dim_size[i];
	    for (j = zloc; j < ndims; j++) {
	       if (orig_dim_size[j] == 0) {
		  /*
		   * Found another zero, compare with assigned value for
		   * previous zero
		   */
		  new_val = dim_size[j];
		  if (new_val > zval)
		     error++;
		  zval = new_val;
	       }
	    }
	 }
	 if (error) {
	    sprintf(info_buf, "Error:  system assigned values increase in   %d  case", error);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    fail++;
	 }

      }
      else {
	 /* Allow printout of why this communicator skipped - at INFO1 level */
	 if (MPITEST_current_rank == 0) {
	    if (inter_comm) {
	       sprintf(info_buf, "Skipped inter-communicator");
	       MPITEST_message(MPITEST_INFO1, info_buf);
	    }
	    else {
	       sprintf(info_buf, "Skipping:  Communicator smaller than minimum  %d/%d", test_nump, MINNODE);
	       MPITEST_message(MPITEST_INFO1, info_buf);
	    }
	 }

      }			/* node defined for this communicator  */

#ifdef MPI_TEST_SYNC

      ierr = MPI_Barrier(MPI_COMM_WORLD);
      if (ierr != MPI_SUCCESS) {
	 sprintf(info_buf, "Non-zero return code (%d) from MPI_Barrier", ierr);
	 MPITEST_message(MPITEST_NONFATAL, info_buf);
	 MPI_Error_string(ierr, error_string, &size);
	 MPITEST_message(MPITEST_NONFATAL, error_string);
	 fail++;
      }
#endif

      MPITEST_free_communicator(comm_type, &comm);

   }			/* Loop over Communicators  */

   /* report overall results  */

   MPITEST_report(loop_cnt - fail, fail, 0, testname);

   MPI_Finalize();

   return fail;

}			/* main() */
