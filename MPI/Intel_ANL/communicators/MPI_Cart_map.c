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

                          MPI_Cart_map

 Reference:  MPI Standard,  Chapter 6:  Process Topologies
                            Section 6.5.7:  Low-level topology functions

 Uses only intra-communicators,  all inter-communicators are skipped
 Must have a minimum of MINNODE nodes in the communicator
 The test program is limited to testing not more than MAXNDIMS dimensions


******************************************************************************/

#define  MPITEST_MAXNDIMS       4	/* Maximum dimensions to generate    */
#define  MPITEST_MINNODE        6	/* Minimum node count to run test    */


#include "mpitest_cfg.h"
#include "mpitest.h"

int main(int argc, char *argv[])
{
   int
      i, j, k, m, n,		/* General loop indices                              */
      comm_count,		/* loop counter for communicator loop                */
      comm_index,		/* the array index of the current comm               */
      comm_type,		/* the index of the current communicator type        */
      error,			/* errors from one MPI call                          */
      fail,			/* counts total number of failures                   */
      ierr,			/* return value from MPI calls                       */
      inter_comm,		/* Intercommunicator flag, true if
				 * intercommunicator */
      ndims,			/* Number of dimensions to generate
				 * coordinates for  */
      nnodes,			/* node count for Dims_create                        */
      reorder,			/* rank reorder flag                                 */
      size,			/* return size from MPI_Error_string                 */
      test_nump,		/* The number of processors in current
				 * communicator  */
      cart_dims,		/* dimensions returned by Cartdim_get                */
      cart_nump,		/* number of ranks in Cartesian communicator         */
      rank_cart,		/* rank in the Cartesian communicator                */
      world_nump,		/* number of processes in MPI_COMM_WORLD             */
      test_val,			/* communication test value                          */
      newrank,			/* rank returned by MPI_Cart_map                     */
      loop_cnt;			/* test event count                                  */

   int
      counter[MPITEST_MAXNDIMS],/* counter for testing coords        */
      coords[MPITEST_MAXNDIMS],	/* periodic assignment flag vector   */
      periods[MPITEST_MAXNDIMS],/* periodic assignment flag vector   */
      dim_size[MPITEST_MAXNDIMS];	/* Cartesian dimension sizes         */

   int
      ranks[2],			/* sorc and dest ranks               */
      all_ranks[MPITEST_MAX_RANKS][2];	/* sorc/dest ranks for all nodes     */

   char
      info_buf[256],		/* buffer for passing mesages to MPITEST             */
      testname[64];		/* the name of this test                             */
    char error_string[MPI_MAX_ERROR_STRING];

   MPI_Comm
      comm,			/* MPITEST communicator                              */
      comm_cart;		/* Cartesian communicator                            */

   /*------------------------------  MPI_Init  -----------------------------*/
   ierr = MPI_Init(&argc, &argv);
   if (ierr != MPI_SUCCESS) {
      sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr);
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

   sprintf(testname, "MPI_Cart_map");

   /*-----------------------------  MPITEST_init  --------------------------*/
   MPITEST_init(argc, argv);

   if (MPITEST_me == 0) {
      sprintf(info_buf, "Starting %s test", testname);
      MPITEST_message(MPITEST_INFO0, info_buf);
   }

      /* Can't run if not at least  MPITEST_MINNODE  nodes  */

      MPI_Comm_size(MPI_COMM_WORLD, &world_nump);
      if (world_nump < MPITEST_MINNODE) {
	 fail++;
	 sprintf(info_buf, "ERROR --  nodes = %3d   Need %3d nodes to run test", world_nump, MPITEST_MINNODE);
	 MPITEST_message(MPITEST_FATAL, info_buf);
      }

   /* set the global error counter */
   fail = 0;
   loop_cnt = 0;
   test_val = -1;

   /*----------------------  Loop over Communicators  ----------------------*/

   for (comm_count = 0; comm_count < MPITEST_num_comm_sizes(); comm_count++) {
      comm_index = MPITEST_get_comm_index(comm_count);
      comm_type = MPITEST_get_comm_type(comm_count);
      test_nump = MPITEST_get_communicator(comm_type, comm_index, &comm);

      if (MPITEST_current_rank != MPI_UNDEFINED) {
	 /* ...  inter_comm is true if  inter-communicator ... */

	 ierr = MPI_Comm_test_inter(comm, &inter_comm);
	 if (ierr != MPI_SUCCESS) {
	    fail++;
	    sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_test_inter", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_FATAL, error_string);
	 }

	 if (!inter_comm && test_nump >= MPITEST_MINNODE) {

	    /* Decide how many dimensions to generate  */

	    ndims = MPITEST_MAXNDIMS;

	    while (test_nump % ndims != 0)
	       ndims--;

	    for (i = 0; i < ndims; i++) {
	       dim_size[i] = 0;
	    }

	    /*-------------------  MPI_Dims_create  ---------------------*/

	    nnodes = test_nump;

	    ierr = MPI_Dims_create(nnodes, ndims, dim_size);

	    if (ierr != MPI_SUCCESS) {
	       fail++;
	       sprintf(info_buf, "MPI_Dims_create error (%d)", ierr);
	       MPITEST_message(MPITEST_NONFATAL, info_buf);
	       MPI_Error_string(ierr, error_string, &size);
	       MPITEST_message(MPITEST_FATAL, error_string);
	    }

	    for (i = 0; i < ndims; i++) {
	       periods[i] = TRUE;
	    }

	    /* Adjust dimensions, so last dimension is not 1  */
	    i = ndims - 1;
	    while (dim_size[i] <= 1 && i >= 0) {
	       dim_size[i] = 0;
	       ndims--;
	       i--;
	    }

	    /*---------------------  MPI_Cart_create  -------------------*/
	    reorder = TRUE;

	    ierr = MPI_Cart_create(comm,
				   ndims,
				   dim_size,
				   periods,
				   reorder,
				   &comm_cart);

	    if (ierr != MPI_SUCCESS) {
	       sprintf(info_buf, "Non-zero return code (%d) from MPI_Cart_create", ierr);
	       MPITEST_message(MPITEST_NONFATAL, info_buf);
	       MPI_Error_string(ierr, error_string, &size);
	       MPITEST_message(MPITEST_FATAL, error_string);
	       fail++;
	    }			/* Error Test  */

	    MPI_Comm_size(comm_cart, &cart_nump);
	    MPI_Comm_rank(comm_cart, &rank_cart);

	    if (rank_cart == 0)
	       test_val = 1;

	    /* Broacast from node zero of Cartesian communicator */

	    ierr = MPI_Bcast(&test_val, 1, MPI_INT, 0, comm_cart);
	    if (ierr != MPI_SUCCESS) {
	       sprintf(info_buf, "Non-zero return code (%d) from MPI_Bcast", ierr);
	       MPITEST_message(MPITEST_NONFATAL, info_buf);
	       MPI_Error_string(ierr, error_string, &size);
	       MPITEST_message(MPITEST_FATAL, error_string);
	       fail++;
	    }			/* Error Test  */

	    /* nodes in communicator check for broadcast value */

	    if (test_val != 1) {
	       sprintf(info_buf, "Error in broadcast  value:  expected 1   actual = %d", test_val);
	       MPITEST_message(MPITEST_FATAL, info_buf);
	       fail++;
	    }

	    /*------------------------ MPI_Cart_map   -------------------------*/

	    loop_cnt++;

	    ierr = MPI_Cart_map(comm_cart,
				ndims,
				dim_size,
				periods,
				&newrank);

	    if (ierr != MPI_SUCCESS) {
	       fail++;
	       sprintf(info_buf, "MPI_Cart_map (%d)", ierr);
	       MPITEST_message(MPITEST_NONFATAL, info_buf);
	       MPI_Error_string(ierr, error_string, &size);
	       MPITEST_message(MPITEST_FATAL, error_string);
	    }

	    /* Send Cartesian rank and newrank to Cartesian node 0  */
	    ranks[0] = rank_cart;
	    ranks[1] = newrank;

	    ierr = MPI_Gather(ranks, 2, MPI_INT, all_ranks, 2, MPI_INT, 0, comm_cart);

	    if (ierr != MPI_SUCCESS) {
	       fail++;
	       sprintf(info_buf, "MPI_Gather error (%d)", ierr);
	       MPITEST_message(MPITEST_NONFATAL, info_buf);
	       MPI_Error_string(ierr, error_string, &size);
	       MPITEST_message(MPITEST_FATAL, error_string);
	    }
	    /* Now, Cartesian node 0 can compare all the ranks  */
	    if (rank_cart == 0) {
	       /* Make sure there are cart_nump distinct ranks in the reorder  */
	       for (j = 0; j < cart_nump; j++) {
		  k = 0;
		  while (j != all_ranks[k][1] && k < cart_nump)
		     k++;
		  if (k >= cart_nump) {

		     sprintf(info_buf, "ERROR:  Missing rank in new mapping  loc = %d  Old/New = (%d/%d) ", j, all_ranks[j][0], all_ranks[j][1]);
		     MPITEST_message(MPITEST_FATAL, info_buf);
		  }

	       }

	    }			/* End of rank_cart == 0 code  */

	    MPI_Comm_free(&comm_cart);

	 }			/* end of MPITEST communicator code  */

	 else {
	    /* intra-communicator  or insufficient-nodes */

	    if (MPITEST_current_rank == 0) {
	       if (inter_comm) {
		  sprintf(info_buf, "Skipped inter-communicator");
		  MPITEST_message(MPITEST_INFO1, info_buf);
	       }
	       else {
		  sprintf(info_buf, "Skipping:  Communicator smaller than minimum  %d/%d", test_nump, MPITEST_MINNODE);
		  MPITEST_message(MPITEST_INFO1, info_buf);
	       }

	    }

	 }


      }				/* node defined for this MPITEST communicator  */

#ifdef MPITEST_SYNC

      ierr = MPI_Barrier(MPI_COMM_WORLD);
      if (ierr != MPI_SUCCESS) {
	 sprintf(info_buf, "Non-zero return code (%d) from MPI_Barrier", ierr);
	 MPITEST_message(MPITEST_NONFATAL, info_buf);
	 MPI_Error_string(ierr, error_string, &size);
	 MPITEST_message(MPITEST_FATAL, error_string);
	 fail++;
      }
#endif

      MPITEST_free_communicator(comm_type, &comm);

   }				/* Loop over Communicators  */

   /* report overall results  */

   MPITEST_report(loop_cnt - fail, fail, 0, testname);

   MPI_Finalize();

   return fail;

}				/* main() */
