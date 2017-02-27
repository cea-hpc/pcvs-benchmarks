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

                         MPI_Graph_get_err3

 Reference:  MPI Standard,  Chapter 6:  Process Topologies
                            Section 6.5.4:  Topology inquiry functions

*  MPI_Graph_get error tests
*  -----------------------------------
* 1)  Call with non-graphic communicator (MPI_COMM_WORLD) [COMM]
* 2)  Call with MPI_COMM_NULL ........................... [COMM]
* 3)  Call with Cartesian communicator .................. [COMM]

****************************************************************************/
#define  TEST_DESC    "MPI_Graph_get called with Cartesian communicator"
#define  EXPECT_ERR      MPI_ERR_TOPOLOGY
#define  ERR_NUM         10


#include "mpitest_cfg.h"
#include "mpitest.h"

int main(int argc, char *argv[])
{
   int
      i, j,			/* General loop indices                              */
      error,			/* errors from one MPI call                          */
      errorclass,		/* error class of ierr                               */
      pass,			/* passed event count                                */
      fail,			/* counts total number of failures                   */
      ierr,			/* return value from MPI calls                       */
      nnodes,			/* number of nodes for MPI_Graph_get                 */
      num_dims,			/* Number of dimensions to generate
				 * coordinates for  */
      size,			/* return size from MPI_Error_string                 */
      test_nump,		/* The number of processors in current
				 * communicator  */
      maxindex,			/* length of index vector                            */
      maxedges,			/* length of edges vector                            */
      nedges,			/* number of edges in graph                          */
      reorder,			/* rank reorder flag                                 */
      rank_cart;		/* rank in graph  communicator                       */



   int
      dim_size[2],		/* Cartesian dimension sizes                         */
      periods[2],		/* allow periodic node assignment flags              */
      index[2],			/* cumulative neighbor count vector                  */
      edges[2],			/* node neighbor vector                              */
      graph_nodes[2],		/* node indices returned from Graph_get              */
      graph_edges[2];		/* graph edges  returned by Graph_get                */


   char
      info_buf[256],		/* buffer for passing mesages to MPITEST             */
      testname[64];		/* the name of this test                             */
    char error_string[MPI_MAX_ERROR_STRING];

   MPI_Comm
      comm,			/* MPI communicator                                  */
      comm_cart,		/* Cartesian communicator                            */
      comm_graph;		/* graph communicator                                */

   /*------------------------------  MPI_Init  -----------------------------*/
   ierr = MPI_Init(&argc, &argv);
   if (ierr != MPI_SUCCESS) {
      sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr);
      MPITEST_message(MPITEST_FATAL, info_buf);
   }
   sprintf(testname, "Graph_get_err3:  called with Cartesian communicator ");

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
      sprintf(info_buf, "MPI_Comm_size() returned %d", ierr);
      MPITEST_message(MPITEST_NONFATAL, info_buf);
      MPI_Error_string(ierr, error_string, &size);
      MPITEST_message(MPITEST_FATAL, error_string);
   }

   /* We need 2 nodes     */

   if (test_nump < 2) {
      sprintf(info_buf, "Not enough ranks to test MPI_Graph_get, need at least 2");
      MPITEST_message(MPITEST_FATAL, info_buf);
   }
   num_dims = 1;
   dim_size[0] = 2;

   /*-----------------------------  MPI_Cart_create  ---------------------------*/

   reorder = TRUE;
   periods[0] = TRUE;

   ierr = MPI_Cart_create(MPI_COMM_WORLD,
			  num_dims,
			  dim_size,
			  periods,
			  reorder,
			  &comm_cart);

   if (ierr != MPI_SUCCESS) {
      fail++;
      MPI_Error_class(ierr, &errorclass);
      sprintf(info_buf, "MPI_Cart_create error (%d)  Errorclass = %d", ierr, errorclass);
      MPITEST_message(MPITEST_NONFATAL, info_buf);
      MPI_Error_string(ierr, error_string, &size);
      MPITEST_message(MPITEST_NONFATAL, error_string);
   }

   if (comm_cart != MPI_COMM_NULL) {

      ierr = MPI_Comm_rank(comm_cart, &rank_cart);
      if (ierr != MPI_SUCCESS) {
	 fail++;
	 sprintf(info_buf, "MPI_Comm_rank() returned %d", ierr);
	 MPITEST_message(MPITEST_NONFATAL, info_buf);
	 MPI_Error_string(ierr, error_string, &size);
	 MPITEST_message(MPITEST_FATAL, error_string);
      }

      if (rank_cart == 0) {

	 /*------------------------- MPI_Graph_get   ----------------------------*/

	 maxindex = 2;
	 maxedges = 2;

	 ierr = MPI_Graph_get(comm_cart,
			      maxindex,
			      maxedges,
			      graph_nodes,
			      graph_edges);

	 if (ierr == MPI_SUCCESS) {
	    fail++;
	    sprintf(info_buf, "%s  did not FAIL", TEST_DESC);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	 }
	 else {
	    MPI_Error_class(ierr, &errorclass);
	    if (errorclass != MPI_ERR_TOPOLOGY && errorclass != MPI_ERR_OTHER) {
	       fail++;
	       sprintf(info_buf, "%s  returned %d(%s), expected MPI_ERR_TOPOLOGY(%d) or MPI_ERR_OTHER(%d)", TEST_DESC, errorclass, MPITEST_GetErrName( errorclass ), MPI_ERR_TOPOLOGY, MPI_ERR_OTHER);
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

   /* report overall results  */

   MPITEST_report(pass, fail, 0, testname);

   MPI_Finalize();
   return fail;
}				/* main() */
