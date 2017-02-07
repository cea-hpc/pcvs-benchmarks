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

                          MPI_Cart_sub

        Reference:  MPI Standard,  Chapter 6:  Process Topologies
                    Section 6.5.6:  Partitioning of Cartesian structures

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
      sub_nump,		/* number of ranks in Sub-communicator               */
      rank_sub,		/* rank in the Sub-communicator                      */
      test_val,		/* communication test value                          */
      dim_rem,		/* new dimensions from Cart_sub                      */
      sub_dims,         /* subset dimension count                            */
      loop_cnt;		/* test event count                                  */

   int
      rem_dims[MPITEST_MAXNDIMS],	/* counter for testing coords        */
      coords[MPITEST_MAXNDIMS],  	/* periodic assignment flag vector   */
      periods[MPITEST_MAXNDIMS],        /* periodic assignment flag vector   */
      dim_size[MPITEST_MAXNDIMS],	/* Cartesian dimension sizes         */
      sub_coords[MPITEST_MAXNDIMS],	/* periodic assignment flag vector   */
      sub_periods[MPITEST_MAXNDIMS],	/* periodic assignment flag vector   */
      test_dim_size[MPITEST_MAXNDIMS],	/* Sub-comm computed dimension sizes */
      sub_dim_size[MPITEST_MAXNDIMS];	/* Sub-comm actual dimension sizes   */


   char
      info_buf[256],	/* buffer for passing mesages to MPITEST             */
      testname[64];	/* the name of this test                             */
    char error_string[MPI_MAX_ERROR_STRING];

   MPI_Comm
      comm,		/* MPITEST communicator                              */
      comm_cart,	/* Cartesian communicator                            */
      comm_sub;		/* Sub communicator                                  */
   /*------------------------------  MPI_Init  -----------------------------*/
   ierr = MPI_Init(&argc, &argv);
   if (ierr != MPI_SUCCESS) {
      sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr);
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

   sprintf(testname, "MPI_Cart_sub:");

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
	    sub_dims = ndims - 1;

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

	    /*---------------------- MPI_Cart_sub   ------------------------*/

	    /* First test the condition where no dimensions are deleted  */

	    loop_cnt++;

	    for (i = 0; i < ndims; i++)
	       rem_dims[i] = TRUE;

	    ierr = MPI_Cart_sub(comm_cart,
				rem_dims,
				&comm_sub);

	    if (ierr != MPI_SUCCESS) {
	       fail++;
	       sprintf(info_buf, "MPI_Cart_sub (%d)", ierr);
	       MPITEST_message(MPITEST_NONFATAL, info_buf);
	       MPI_Error_string(ierr, error_string, &size);
	       MPITEST_message(MPITEST_FATAL, error_string);
	    }

	    ierr = MPI_Comm_rank(comm_sub, &rank_sub);
	    if (ierr != MPI_SUCCESS) {
	       fail++;
	       sprintf(info_buf, "MPI_Comm_rank error (%d)", ierr);
	       MPITEST_message(MPITEST_NONFATAL, info_buf);
	       MPI_Error_string(ierr, error_string, &size);
	       MPITEST_message(MPITEST_FATAL, error_string);
	    }

	    /*----------------------- MPI_Cart_get   ------------------------*/

	    ierr = MPI_Cart_get(comm_sub,
				ndims,
				sub_dim_size,
				sub_periods,
				sub_coords);

	    if (ierr != MPI_SUCCESS) {
	       fail++;
	       sprintf(info_buf, "MPI_Cart_get error (%d)", ierr);
	       MPITEST_message(MPITEST_NONFATAL, info_buf);
	       MPI_Error_string(ierr, error_string, &size);
	       MPITEST_message(MPITEST_FATAL, error_string);
	    }

	    if (rank_sub == 0)
	       test_val = 1;

	    /* Broacast from node zero of Sub-communicator */

	    ierr = MPI_Bcast(&test_val, 1, MPI_INT, 0, comm_sub);
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

	    MPI_Comm_free(&comm_sub);
            test_val = -1;
	    /*----------------------- MPI_Cart_sub   -----------------------*/

	    if (ndims > 1) {

	       loop_cnt++;

	       /* Remove each dimension and test result  */

	       for (j = 0; j < ndims; j++) {
		  dim_rem = dim_size[j];

		  for (k = 0; k < ndims; k++) {
		     rem_dims[k] = TRUE;

		  }

		  rem_dims[j] = FALSE;

		  m = 0;
		  for (k = 0; k < ndims; k++) {
		     if (k == j)
			continue;
		     test_dim_size[m] = dim_size[k];
		     m++;
		  }

		  ierr = MPI_Cart_sub(comm_cart,
				      rem_dims,
				      &comm_sub);

		  if (ierr != MPI_SUCCESS) {
		     fail++;
		     sprintf(info_buf, "MPI_Cart_sub (%d)", ierr);
		     MPITEST_message(MPITEST_NONFATAL, info_buf);
		     MPI_Error_string(ierr, error_string, &size);
		     MPITEST_message(MPITEST_FATAL, error_string);
		  }

		  MPI_Comm_size(comm_sub, &sub_nump);
		  MPI_Comm_rank(comm_sub, &rank_sub);

		  /*--------------------- MPI_Cart_get   --------------------*/


		  ierr = MPI_Cart_get(comm_sub,
				      sub_dims,
				      sub_dim_size,
				      sub_periods,
				      sub_coords);

		  if (ierr != MPI_SUCCESS) {
		     fail++;
		     sprintf(info_buf, "MPI_Cart_get error (%d)", ierr);
		     MPITEST_message(MPITEST_NONFATAL, info_buf);
		     MPI_Error_string(ierr, error_string, &size);
		     MPITEST_message(MPITEST_FATAL, error_string);
		  }

		  if (rank_sub == 0)
		     test_val = 1;

		  /* Broacast from node zero of Sub-communicator */

		  ierr = MPI_Bcast(&test_val, 1, MPI_INT, 0, comm_sub);
		  if (ierr != MPI_SUCCESS) {
		     sprintf(info_buf, "Non-zero return code (%d) from MPI_Bcast", ierr);
		     MPITEST_message(MPITEST_NONFATAL, info_buf);
		     MPI_Error_string(ierr, error_string, &size);
		     MPITEST_message(MPITEST_FATAL, error_string);
		     fail++;
		  }	/* Error Test  */

		  /* nodes in communicator check for broadcast value */

		  if (test_val != 1) {
		     sprintf(info_buf, "Error in broadcast  value:  expected 1   actual = %d", test_val);
		     MPITEST_message(MPITEST_NONFATAL, info_buf);
		     fail++;
		  }

		  /* check sub_communicator dimensions */
		  for (k = 0; k < sub_dims; k++) {
		     if (sub_dim_size[k] != test_dim_size[k]) {
			sprintf(info_buf, "Error in Sub-communicator dimensions for dimension %3d:  expected %3d   actual = %d", k, test_dim_size[k], sub_dim_size[k]);
			MPITEST_message(MPITEST_NONFATAL, info_buf);
			fail++;
		     }
		  }
		  MPI_Comm_free(&comm_sub);

	       }	/* j loop over dimensions  */
	    }		/* ndims > 1 */

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
