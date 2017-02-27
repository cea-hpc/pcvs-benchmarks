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
                          MPI_Graph_create

 Reference:  MPI Standard,  Chapter 6:  Process Topologies
                            Section 6.5.2

 Uses only intra-communicators,  all inter-communicators are skipped
 Must have a minimum of MINNODE nodes in the communicator
 The test program is limited to testing not more than MAXNDIMS dimensions

 Model  Test with  reorder = TRUE

******************************************************************************/

#define  MPITEST_MAXNDIMS    4	/* Maximum dimensions to generate            */
#define  MPITEST_MINNODE     6	/* Minimum number of nodes required for test */
#define  MPITEST_MAXN        8	/* maximum number of neighbors for each node */
#include "mpitest_cfg.h"
#include "mpitest.h"

int main(int argc, char *argv[])
{
   int
      i, j,			/* General loop indices                              */
      comm_count,		/* loop counter for communicator loop                */
      comm_index,		/* the array index of the current comm               */
      comm_type,		/* the index of the current communicator type        */
      error,			/* errors from one MPI call                          */
      fail = 0,			/* counts total number of failures                   */
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
      graph_nump = 0,		/* number of ranks in graph communicator             */
      rank_graph,		/* rank in the graph communicator                    */
      rank_world,		/* node rank in MPI_COMM_WORLD                       */
      rank_comm,		/* rank in the comm communicator                     */
      world_nump,		/* number of processes in MPI_COMM_WORLD             */
      test_val,			/* communication test value                          */
      sum,			/* sum of flags  for checking communicator           */
      undefined,		/* flag telling if node is undefined in
				 * communicator */
      loop_cnt;			/* test event count                                  */

   int
      index[MPITEST_MAX_RANKS],	/* node connection count        */
      edges[MPITEST_MAX_RANKS * MPITEST_MAXN];	/* node connection list         */

   char
      info_buf[256],		/* buffer for passing mesages to MPITEST             */
      testname[64];		/* the name of this test                             */
    char error_string[MPI_MAX_ERROR_STRING];

   MPI_Comm
      comm,			/* MPITEST communicator                              */
      comm_graph;		/* graph communicator                                */

   /*------------------------------  MPI_Init  -----------------------------*/
   ierr = MPI_Init(&argc, &argv);
   if (ierr != MPI_SUCCESS) {
      sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr);
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

   sprintf(testname, "MPI_Graph_create with undefined node");

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
	 MPITEST_message(MPITEST_NONFATAL, info_buf);
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

	    /*----------------------  Set up index and edge arrays  ---------------------*/

	    nnodes = test_nump - 1;

	    /* assume a single string of nodes with wraparound  */

	    j = 2;

	    /* Set up edges for node 0  */
	    edges[0] = nnodes - 1;
	    edges[1] = 1;

	    for (i = 1; i < nnodes - 1; i++) {
	       edges[j] = i - 1;
	       edges[j + 1] = i + 1;
	       j = j + 2;
	    }
	    edges[j] = nnodes - 2;
	    j++;

	    edges[j] = 0;
	    for (i = 0; i < nnodes; i++) {
	       index[i] = (i + 1) * 2;
	    }
	    /*-----------------------------  MPI_Graph_create  --------------------------*/

	    loop_cnt++;

	    reorder = TRUE;

	    ierr = MPI_Graph_create(comm,
				    nnodes,
				    index,
				    edges,
				    reorder,
				    &comm_graph);

	    if (ierr != MPI_SUCCESS) {
	       fail++;
	       sprintf(info_buf, "MPI_Graph_create error (%d) ", ierr);
	       MPITEST_message(MPITEST_NONFATAL, info_buf);
	       MPI_Error_string(ierr, error_string, &size);
	       MPITEST_message(MPITEST_NONFATAL, error_string);
	    }

	    MPI_Comm_rank(comm, &rank_comm);

	    undefined = 0;
	    if (comm_graph == MPI_COMM_NULL)
	       undefined = 1;

	    ierr = MPI_Reduce(&undefined, &sum, 1, MPI_INT, MPI_SUM, 0, comm);
	    if (ierr != MPI_SUCCESS) {
	       sprintf(info_buf, "Non-zero return code (%d) from MPI_Reduce", ierr);
	       MPITEST_message(MPITEST_NONFATAL, info_buf);
	       MPI_Error_string(ierr, error_string, &size);
	       MPITEST_message(MPITEST_FATAL, error_string);
	       fail++;
	    }			/* Error Test  */

	    /* If node zero of  the comm communicator check the sum */

	    if (rank_comm == 0) {
	       if (sum != 1) {
		  sprintf(info_buf, "Error:  Number of undefined nodes not 1 sum = %d", sum);
		  MPITEST_message(MPITEST_FATAL, info_buf);
		  fail++;
	       }
	       sprintf(info_buf, "Reduce results:  sum = %d    graph_nump = %d comm_size = %d", sum, graph_nump, test_nump);
	       MPITEST_message(MPITEST_INFO2, info_buf);
	    }

	    if (comm_graph != MPI_COMM_NULL)
	        MPI_Comm_free(&comm_graph);
	 }
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

      }				/* node defined for comm communicator  */

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
