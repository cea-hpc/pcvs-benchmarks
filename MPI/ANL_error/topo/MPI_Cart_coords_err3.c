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
SPECULATIVE DAMAGES, (INCLUDING WITHOUT LIMIT
ING THE FOREGOING, CONSEQUENTIAL,
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

                          MPI_Cart_coords_err3


 Reference:  MPI Standard,  Chapter 6:  Process Topologies
                            Section 6.5.4:  Topology inquiry functions


*  MPI_Cart_coords error tests
*  -----------------------------------
* 1)  Communicator with no topology (MPI_COMM_WORLD) .... [COMM/TOPOLOGY/ARG]
* 2)  Call with MPI_COMM_NULL ........................... [COMM/TOPOLOGY/ARG]
* 3)  Call with graph communicator ...................... [COMM/TOPOLOGY/ARG]

****************************************************************************/
#define  DIMENSIONS      1
#define  TEST_DESC       "MPI_Cart_coords:  call with graph communicator"
#define  EXPECT_ERR      MPI_ERR_COMM


#include "mpitest_cfg.h"
#include "mpitest.h"


int main(int argc, char *argv[])
{
   int
      i,			/* General loop indices                              */
      error,			/* errors from one MPI call                          */
      errorclass,		/* error class of ierr                               */
      pass,			/* event pass count                                  */
      fail,			/* counts total number of failures                   */
      ierr,			/* return value from MPI calls                       */
      num_dims,			/* Number of dimensions to generate
				 * coordinates for  */
      size,			/* return size from MPI_Error_string                 */
      test_nump,		/* The number of processors in current
				 * communicator  */
      rank_graph,		/* process rank within graph communicator            */
      nnodes,			/* node count for Graph_create                       */
      reorder;			/* allow rank reorder flag                           */

   int
      index[2],			/* Cumulative connection count vector         */
      edges[2],			/* Node connection list vector                */
      coords[DIMENSIONS],	/* Cartesian coordinates of calling process   */
      dim_size[DIMENSIONS],	/* number of processes in each dimension      */
      periods[DIMENSIONS];	/* periodic flag for each coordinate
				 * dimension */

   char
      info_buf[256],		/* buffer for passing mesages to MPITEST             */
      testname[64];		/* the name of this test                             */
    char error_string[MPI_MAX_ERROR_STRING];

   MPI_Comm
      comm_cart,		/* Cartesian communicator                            */
      comm_graph;		/* Graph     communicator                            */

   /*------------------------------  MPI_Init  -----------------------------*/
   ierr = MPI_Init(&argc, &argv);
   if (ierr != MPI_SUCCESS) {
      sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr);
      MPITEST_message(MPITEST_FATAL, info_buf);
   }
   sprintf(testname, "MPI_Cart_coords_err3:  Call with graph communicator ");

   /*-----------------------------  MPITEST_init  --------------------------*/
   MPITEST_init(argc, argv);
   if (MPITEST_me == 0) {
      sprintf(info_buf, "Starting %s test", testname);
      MPITEST_message(MPITEST_INFO0, info_buf);
   }
   /* set the global error counter */
   fail = 0;
   pass = 0;

   /*-----------  Set an errorhandler so we get control back  --------------*/

   ierr = MPI_Errhandler_set(MPI_COMM_WORLD, MPI_ERRORS_RETURN);

   if (ierr != MPI_SUCCESS) {
      fail++;
      sprintf(info_buf, "MPI_Errorhandler_set returned %d", ierr);
      MPITEST_message(MPITEST_NONFATAL, info_buf);
      MPI_Error_string(ierr, error_string, &size);
      MPITEST_message(MPITEST_FATAL, error_string);
   }

   ierr = MPI_Comm_size(MPI_COMM_WORLD, &test_nump);
   if (ierr != MPI_SUCCESS) {
      fail++;
      sprintf(info_buf, "MPI_Comm_size returned %d", ierr);
      MPITEST_message(MPITEST_NONFATAL, info_buf);
      MPI_Error_string(ierr, error_string, &size);
      MPITEST_message(MPITEST_FATAL, error_string);
   }

   /* We need 2 nodes     */

   if (test_nump < 2) {
      sprintf(info_buf, "Not enough ranks to test MPI_Cart_coords, need at least 2");
      MPITEST_message(MPITEST_FATAL, info_buf);
   }
   num_dims = 1;
   nnodes = 2;
   index[0] = 1;
   index[1] = 2;
   edges[0] = 1;
   edges[1] = 0;

   /*-----------------------------  MPI_Graph_create  --------------------------*/

   reorder = TRUE;

   ierr = MPI_Graph_create(MPI_COMM_WORLD,
			   nnodes,
			   index,
			   edges,
			   reorder,
			   &comm_graph);

   if (ierr != MPI_SUCCESS) {
      fail++;
      MPI_Error_class(ierr, &errorclass);
      sprintf(info_buf, "MPI_Graph_create error (%d)  Errorclass = %d", ierr, errorclass);
      MPITEST_message(MPITEST_NONFATAL, info_buf);
      MPI_Error_string(ierr, error_string, &size);
      MPITEST_message(MPITEST_FATAL, error_string);
   }

   if (comm_graph != MPI_COMM_NULL) {
      ierr = MPI_Comm_rank(comm_graph, &rank_graph);
      if (ierr != MPI_SUCCESS) {
	 fail++;
	 MPI_Error_class(ierr, &errorclass);
	 sprintf(info_buf, "MPI_Comm_rank() error (%d)  Errorclass = %d", ierr, errorclass);
	 MPITEST_message(MPITEST_NONFATAL, info_buf);
	 MPI_Error_string(ierr, error_string, &size);
	 MPITEST_message(MPITEST_FATAL, error_string);
      }

      if (rank_graph == 0) {
	 /*--------------------------- MPI_Cart_coords   ----------------------------*/

	 ierr = MPI_Cart_coords(comm_graph,
				0,
				num_dims,
				coords);

	 if (ierr == MPI_SUCCESS) {
	    fail++;
	    sprintf(info_buf, "%s  did not FAIL", TEST_DESC);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	 }
	 else {
	    MPI_Error_class(ierr, &errorclass);
	    if (errorclass != MPI_ERR_COMM && errorclass != MPI_ERR_TOPOLOGY && errorclass != MPI_ERR_ARG && errorclass != MPI_ERR_OTHER) {
	       fail++;
	       sprintf(info_buf, "%s  returned %d, expected MPI_ERR_COMM(%d) or MPI_ERR_ARG(%d) or MPI_ERR_TOPOLOGY(%d) or MPI_ERR_OTHER(%d)", TEST_DESC, errorclass, MPI_ERR_COMM, MPI_ERR_ARG, MPI_ERR_TOPOLOGY, MPI_ERR_OTHER);
	       MPITEST_message(MPITEST_NONFATAL, info_buf);
	       MPI_Error_string(ierr, error_string, &size);
	       MPITEST_message(MPITEST_NONFATAL, error_string);
	    }
	    else {
	       pass++;
	       sprintf(info_buf, "ierr = %d, errorclass = %d", ierr,
		       errorclass);
	       MPITEST_message(MPITEST_INFO2, info_buf);
	       MPI_Error_string(ierr, error_string, &size);
	       MPITEST_message(MPITEST_INFO1, error_string);
	    }
	 }
      }
   }

   MPITEST_report(pass, fail, 0, testname);
   MPI_Finalize();
   return fail;
}				/* main() */
