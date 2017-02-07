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

                          MPI_Cart_rank

        Reference:  MPI Standard,  Chapter 6:  Process Topologies
                                   Section 6.5.4 Topology inquiry functions

 Uses only intra-communicators,  all inter-communicators are skipped
 Must have a minimum of MINNODE nodes in the communicator
 The test program is limited to testing not more than MAXNDIMS dimensions

 The program Calls the MPI_Cart_rank function for each coordinate combination
 in the current Cartesian topology. The rank returned by each call is  checked
 for being within limits.  In addition a sum total of all the ranks is kept.
 The sum of ranks value is compared with a calculated value.
******************************************************************************/

#define  MPITEST_MAXNDIMS    4	/* Maximum dimensions to generate            */
#define  MPITEST_MINNODE     6	/* Minimum number of nodes required for test */


#include "mpitest_cfg.h"
#include "mpitest.h"

int main(int argc, char *argv[])
{
   int
      i, j, k, m, n,	/* General loop indices                              */
      comm_count,	/* loop counter for communicator loop                */
      comm_index,	/* the array index of the current comm               */
      comm_type,	/* the index of the current communicator type        */
      error,		/* errors from one MPI call                          */
      fail,		/* counts total number of failures                   */
      ierr,		/* return value from MPI calls                       */
      inter_comm,	/* Intercommunicator flag, true if intercommunicator */
      ndims,		/* Number of dimensions to generate coordinates for  */
      nnodes,		/* node count for Dims_create                        */
      reorder,		/* rank reorder flag                                 */
      size,		/* return size from MPI_Error_string                 */
      test_nump,	/* The number of processors in current communicator  */
      cart_dims,	/* dimensions returned by Cartdim_get                */
      cart_nump,	/* number of ranks in Cartesian communicator         */
      test_rank,	/* rank in current communicator                      */
      rank_cart,	/* rank in the Cartesian communicator                */
      test_val,		/* communication test value                          */
      loop_cnt;		/* test event count                                  */

   int
      ranks[MPITEST_MAX_RANKS],           /* list of ranks                     */
      counter[MPITEST_MAXNDIMS],        /* counter for testing coords        */
      coords[MPITEST_MAXNDIMS],	        /* periodic assignment flag vector   */
      periods[MPITEST_MAXNDIMS],        /* periodic assignment flag vector   */
      dim_size[MPITEST_MAXNDIMS];	/* Cartesian dimension sizes         */

   char
      info_buf[256],	/* buffer for passing mesages to MPITEST             */
      testname[64];	/* the name of this test                             */
    char error_string[MPI_MAX_ERROR_STRING];

   MPI_Comm
      comm,		/* MPITEST communicator                              */
      comm_cart;	/* Cartesian communicator                            */

   /*------------------------------  MPI_Init  -----------------------------*/
   ierr = MPI_Init(&argc, &argv);
   if (ierr != MPI_SUCCESS) {
      sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr);
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

   sprintf(testname, "MPI_Cart_rank  ");

   /*-----------------------------  MPITEST_init  --------------------------*/
   MPITEST_init(argc, argv);

   if (MPITEST_me == 0) {
      sprintf(info_buf, "Starting %s test", testname);
      MPITEST_message(MPITEST_INFO0, info_buf);
   }

      /* Can't run if not at least  MPITEST_MINNODE  nodes  */

      if (MPITEST_nump < MPITEST_MINNODE) {
	 fail++;
	 sprintf(info_buf, "ERROR --  nodes = %3d   Need %3d nodes to run test", MPITEST_nump, MPITEST_MINNODE);
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

	    ierr = MPI_Dims_create(nnodes,
				   ndims,
				   dim_size);

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
	    }		/* Error Test  */

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
	    }		/* Error Test  */

	    /* nodes in communicator check for broadcast value */

	    if (test_val != 1) {
	       sprintf(info_buf, "Error in broadcast  value:  expected 1   actual = %d", test_val);
	       MPITEST_message(MPITEST_FATAL, info_buf);
	       fail++;
	    }

	    /*---------------------- MPI_Cart_rank   ------------------------*/

	    /* Rank 0 of the Cartesian communicator can do all of the calls */

	    if (rank_cart == 0) {

	       /*
	        * Loop through all counter values, and see if we get a valid
	        * rank
	        */

	       for (i = 0; i < MPITEST_MAXNDIMS; i++)
		  counter[i] = 0;

	       for(i=0; i<MPITEST_MAX_RANKS; i++)
		  ranks[i] = 0;

	       test_rank = -1;
	       /* just need to call nnodes times */
	       for (k = 0; k < nnodes; k++) {
		  loop_cnt++;

		  ierr = MPI_Cart_rank(comm_cart,
				       counter,
				       &test_rank);

		  if (ierr != MPI_SUCCESS) {
		     fail++;
		     sprintf(info_buf, "MPI_Cartdim_get error (%d)", ierr);
		     MPITEST_message(MPITEST_NONFATAL, info_buf);
		     MPI_Error_string(ierr, error_string, &size);
		     MPITEST_message(MPITEST_FATAL, error_string);
		  }

		  /* error if rank is not in range  */
		  if (test_rank < 0 || test_rank >= nnodes) {
		     sprintf(info_buf, "ERROR  MPI_Cart_rank: (Out of range)  Actual = %3d   Expected = 0 - %3d", test_rank, nnodes);
		     MPITEST_message(MPITEST_FATAL, info_buf);
		  }
		  ranks[test_rank] = 1;

		  sprintf(info_buf, "%5d.   Counter:  %3d%3d%3d%3d    Rank:  %3d ", k, counter[0], counter[1], counter[2], counter[3], test_rank);
		  MPITEST_message(MPITEST_INFO2, info_buf);

		  /* advance the counter  */
		  counter[ndims - 1]++;
		  m = ndims - 1;
		  while (m > 0) {
		     if (counter[m] >= dim_size[m]) {
			counter[m] = 0;
			if (m - 1 >= 0)
			   counter[m - 1]++;
		     }
		     m--;

		  }	/* m-loop to advance counter  */

	       }	/* k indexed for-loop over ranks  */

	       for (i=0; i<nnodes; i++)  {
		 if(ranks[i] != 1) {
		   sprintf(info_buf, "ERROR:  Missing node = %d", i);
		   MPITEST_message(MPITEST_FATAL, info_buf);
		 }
	       }
	    }		/* End of cart_rank = 0  code */
	    MPI_Comm_free(&comm_cart);
	 }		/* end of MPITEST communicator code  */
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


      }			/* node defined for this MPITEST communicator  */

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

   }			/* Loop over Communicators  */

   /* report overall results  */

   MPITEST_report(loop_cnt - fail, fail, 0, testname);

   MPI_Finalize();

   return fail;

}			/* main() */
