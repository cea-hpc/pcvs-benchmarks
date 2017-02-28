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
PARTICULAR PURPOSE AND NON-INFRINGEMENT.  THE SOFTWARE PROVIDED HEREUNDER
IS ON AN "AS IS" BASIS AND INTEL CORPORATION HAS NO OBLIGATION TO PROVIDE
MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS OR MODIFICATIONS.
-----------------------------------------------------------------------------*/
/******************************************************************************

                          MPI_Cart_shift_periodic

 Reference:  MPI Standard,  Chapter 6:  Process Topologies
                            Section 6.5.5: Cartesian Shift Coordinates

 Uses only intra-communicators,  all inter-communicators are skipped
 Must have a minimum of MINNODE nodes in the communicator
 The test program is limited to testing not more than MAXNDIMS dimensions

 The Cartesian communicator with all elements of the period array set to
 true.  This when the MPI_Cart_shift call results in a rank_source and/or
 rank_dest values outside of the dimensions of the dimension bounds, the 
 ranks cycle, end around, in both directions.  A value one below 0 cycles
 to the rank of the upper bound for the dimension, and a value one above
 the rank of the upper bound for the dimension becomes the rank of the
 0 element of the dimension
******************************************************************************/

#define  MPITEST_MAXNDIMS    4	/* Maximum dimensions to generate            */
#define  MPITEST_MINNODE     6	/* Minimum number of nodes required for test */


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
      loop_cnt,			/* test event count                                  */
      dim_max,			/* max dimension value                               */
      sorc_rank,		/* source rank returned from MPI_Cart_shift          */
      exp_sorc,			/* computed sorc value to compare with sorc          */
      sorc,			/* sorc value returned by Cart_shift                 */
      dest_rank,		/* destination rank from Cart_shift                  */
      exp_dest,			/* computed dest value to compare with dest          */
      dest;			/* dest value returned by Cart_shift                 */


   int
      counter[MPITEST_MAXNDIMS],/* counter for testing coords        */
      test_coords[MPITEST_MAXNDIMS],	/* periodic assignment flag vector   */
      coords[MPITEST_MAXNDIMS],	/* periodic assignment flag vector   */
      periods[MPITEST_MAXNDIMS],/* periodic assignment flag vector   */
      dim_size[MPITEST_MAXNDIMS];	/* Cartesian dimension sizes         */

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

   sprintf(testname, "MPI_Cart_shift");

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
	       MPITEST_message(MPITEST_NONFATAL, info_buf);
	       fail++;
	    }

	    /*------------------------ MPI_Cart_shift   ------------------------*/

	    /* Get a coordinate for this rank */

	    ierr = MPI_Cart_coords(comm_cart,
				   rank_cart,
				   ndims,
				   coords);

	    if (ierr != MPI_SUCCESS) {
	       fail++;
	       sprintf(info_buf, "MPI_Cartdim_get error (%d)", ierr);
	       MPITEST_message(MPITEST_NONFATAL, info_buf);
	       MPI_Error_string(ierr, error_string, &size);
	       MPITEST_message(MPITEST_FATAL, error_string);
	    }
	    /* Displace one in each direction for each dimension  */

	    for (j = 0; j < ndims; j++) {
	       dim_max = dim_size[j];
	       if (dim_max <= 1)
		  continue;
	       /* This is the displacement loop  */
	       for (k = -1; k <= 1; k++) {
		  if (k == 0)
		     continue;
		  loop_cnt++;
		  ierr = MPI_Cart_shift(comm_cart, j, k, &sorc, &dest);
		  /* Now determine if this was a proper shift  */

		  if (k < 0) {
		     /*
		      * sorc is coords[j]|k|, MPI_PROC_NULL, or periodic
		      * value
		      */
		     exp_sorc = coords[j] - k;
		     if (exp_sorc >= dim_max && !periods[j])
			exp_sorc = MPI_PROC_NULL;
		     if (exp_sorc >= dim_max && periods[j])
			exp_sorc = exp_sorc - dim_max;
		  }
		  if (k > 0) {
		     /*
		      * sorc is coords[j] - k, MPI_PROC_NULL, or periodic
		      * valu
		      */
		     exp_sorc = coords[j] - k;
		     if (exp_sorc < 0 && !periods[j])
			exp_sorc = MPI_PROC_NULL;
		     if (exp_sorc < 0 && periods[j])
			exp_sorc = exp_sorc + dim_max;
		  }
		  /* now compare with returned source value  */

		  for (m = 0; m < ndims; m++)
		     test_coords[m] = coords[m];
		  test_coords[j] = exp_sorc;
		  MPI_Cart_rank(comm_cart, test_coords, &sorc_rank);
		  if (sorc_rank != sorc) {
		     sprintf(info_buf, "Source (%d/%d)  expected coord/rank (%d/%d)    Actual %d", j, k, exp_sorc, sorc_rank, sorc);
		     MPITEST_message(MPITEST_NONFATAL, info_buf);
		     fail++;
		  }

		  /* dest is coords[j] - k, MPI_PROC_NULL, or periodic value */
		  if (k < 0) {
		     exp_dest = coords[j] + k;
		     if (exp_dest < 0 && !periods[j])
			exp_dest = MPI_PROC_NULL;
		     if (exp_dest < 0 && periods[j])
			exp_dest = exp_dest + dim_max;
		  }

		  /* dest is coords[j] + k, MPI_PROC_NULL, or periodic value */
		  if (k > 0) {
		     exp_dest = coords[j] + k;
		     if (exp_dest >= dim_max && !periods[j])
			exp_dest = MPI_PROC_NULL;
		     if (exp_dest >= dim_max && periods[j])
			exp_dest = exp_dest - dim_max;
		  }

		  /* now compare with returned source value  */

		  for (m = 0; m < ndims; m++)
		     test_coords[m] = coords[m];
		  test_coords[j] = exp_dest;
		  MPI_Cart_rank(comm_cart, test_coords, &dest_rank);
		  if (dest_rank != dest) {
		     sprintf(info_buf, "Dest (%d/%d)  expected coord/rank (%d/%d)    Actual %d", j, k, exp_dest, dest_rank, dest);
		     MPITEST_message(MPITEST_NONFATAL, info_buf);
		     fail++;
		  }

	       }		/* End of k loop over displacements for each
				 * coordinate  */

	    }			/* End of j loop over all dimensions for this
				 * node */

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
