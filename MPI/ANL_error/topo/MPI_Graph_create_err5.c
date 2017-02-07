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

                          MPI_Graph_create_err5

 Reference:  MPI Standard,  Chapter 6:  Process Topologies
                            Section 6.5.3:  General(Graph) Constructor


*  MPI_Graph_create error tests
*  -----------------------------------
* 1)  Nodes < 0 ......................................... [ARG]
* 2)  Call with nodes > MPI_COMM_WORLD .................. [ARG]
* 3)  Incompatible parameters:  Edge number too large ... [ARG/TOPOLOGY]
* 4)  Incompatible parameters:  Negative edge number .... [ARG/TOPOLOGY]
* SELF EDGES ARE LEGAL IN 3.1
* 5)  Incompatible parameters:  Edge to itself .......... [ARG/TOPOLOGY]
* 6)  Call with MPI_COMM_NULL ........................... [COMM]

****************************************************************************/
#define  DIMENSIONS      2
#define  TEST_DESC       "MPI_Graph_create with Null Edge"

#include "mpitest_cfg.h"
#include "mpitest.h"

int main(int argc, char *argv[])
{
   int
      i, j,			/* General loop indices                              */
      error,			/* errors from one MPI call                          */
      errorclass,		/* error class of ierr                               */
      pass,			/* event pass count                                  */
      fail,			/* counts total number of failures                   */
      ierr,			/* return value from MPI calls                       */
      nnodes,			/* number of nodes for MPI_Graph_create              */
      num_dims,			/* Number of dimensions to generate
				 * coordinates for  */
      size,			/* return size from MPI_Error_string                 */
      test_nump,		/* The number of processors in current
				 * communicator  */
      reorder;			/* ranks reorder flag                                */

   int
      index[4],			/* graph cumulative connection count array           */
      edges[6];			/* graph node neighbors list array                   */

   char
      info_buf[256],		/* buffer for passing mesages to MPITEST             */
      testname[64];		/* the name of this test                             */
    char error_string[MPI_MAX_ERROR_STRING];

   MPI_Comm
      comm_graph;		/* graph communicator                                */

   /*------------------------------  MPI_Init  -----------------------------*/
   ierr = MPI_Init(&argc, &argv);
   if (ierr != MPI_SUCCESS) {
      sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr);
      MPITEST_message(MPITEST_FATAL, info_buf);
   }
   sprintf(testname, "Graph_create_err5:  Null edge ");

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

   /* We need 4 nodes, so we can specify  a 2 x 2 grid     */

   if (MPITEST_nump < 4) {
      sprintf(info_buf, "Not enough ranks to test MPI_Graph_create, need at least 4 nodes");
      MPITEST_message(MPITEST_FATAL, info_buf);
   }
   num_dims = DIMENSIONS;
   nnodes = 4;

   edges[1] = 3;
   edges[2] = 0;
   edges[3] = 3;
   edges[4] = 0;
   edges[5] = 2;

   index[0] = 2;
   index[1] = 3;
   index[2] = 4;
   index[3] = 6;

   /*-----------------------------  MPI_Graph_create  ------------------------*/

   edges[0] = 0;

   reorder = TRUE;

   ierr = MPI_Graph_create(MPI_COMM_WORLD,
			   nnodes,
			   index,
			   edges,
			   reorder,
			   &comm_graph);

   if (ierr != MPI_SUCCESS) {
      sprintf(info_buf, "%s  did not FAILED", TEST_DESC);
   }


   MPI_Barrier(MPI_COMM_WORLD);

   /* report overall results  */

   MPITEST_report(pass, fail, 0, testname);

   MPI_Finalize();

   return fail;

}				/* main() */
