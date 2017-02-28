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
                            Section 6.5.2:  Cartesian Convenidence Function

*  MPI_Dims_create error tests
*  -----------------------------------
* 1)  Nodes not a multiple of non-zero dimensions   ..... [DIMS]
* 2)  Negative value in dimensions array  ............... [DIMS]
* 3)  Nodes < 0 ......................................... [ARG]
* 4)  Number of dimensions < 0  ......................... [ARG/DIMS]

******************************************************************************/
#define  MAXDIMS       10
#define  PRNT_NODE      0
#define  DIMENSIONS     4
#define  MAXNODE      120
#define  TEST_DESC    "MPI_Dims_create_Err4:  Called with number of dimensions < 0"
#define  EXPECT_ERR     MPI_ERR_ARG
#define  ERR_NUM       12


#include "mpitest_cfg.h"
#include "mpitest.h"


int main(int argc, char *argv[])
{
   int
      i, j, k, l, m,		/* General loop indices                              */
      my_comm_size,		/* number of nodes in subcommunicator                */
      error,			/* errors from one MPI call                          */
      errorclass,		/* error class of ierr                               */
      fail,			/* counts total number of failures                   */
      pass, ierr,		/* return value from MPI calls                       */
      ntimes,			/* # of communicator groups INTRA=1, INTER=2         */
      nnodes,			/* NUmber of nodes for MPI_Dims_create call          */
      num_dims,			/* Number of dimensions to generate
				 * coordinates for  */
      size;			/* return size from MPI_Error_string                 */

   int
      dim_size[MAXDIMS];	/* Cartesian dimension sizes                */

   char
      info_buf[256],		/* buffer for passing mesages to MPITEST             */
      testname[64];		/* the name of this test                             */
    char error_string[MPI_MAX_ERROR_STRING];

   /*------------------------------  MPI_Init  -----------------------------*/
   ierr = MPI_Init(&argc, &argv);
   if (ierr != MPI_SUCCESS) {
      sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr);
      MPITEST_message(MPITEST_FATAL, info_buf);
   }
   sprintf(testname, "%s", TEST_DESC);

   /*-----------------------------  MPITEST_init  --------------------------*/
   MPITEST_init(argc, argv);
   if (MPITEST_me == 0) {
      sprintf(info_buf, "Starting %s test", testname);
      MPITEST_message(MPITEST_INFO0, info_buf);
   }
   /* set the global error counter */
   pass = 0;
   fail = 0;


   /*-----------  Set an errorhandler so we get control back  --------------*/

   ierr = MPI_Errhandler_set(MPI_COMM_WORLD, MPI_ERRORS_RETURN);

   if (ierr != MPI_SUCCESS) {
      fail++;
      sprintf(info_buf, "MPI_Errorhandler_set returned %d", ierr);
      MPITEST_message(MPITEST_NONFATAL, info_buf);
      MPI_Error_string(ierr, error_string, &size);
      MPITEST_message(MPITEST_FATAL, error_string);
   }

   nnodes = 120;
   num_dims = -1;

   dim_size[0] = 4;
   dim_size[1] = 5;
   dim_size[2] = 1;
   dim_size[3] = 6;


   /*---------------------------  MPI_Dims_create  --------------------------*/

   ierr = MPI_Dims_create(nnodes,
			  num_dims,
			  dim_size);

   if (ierr == MPI_SUCCESS) {
      fail++;
      sprintf(info_buf, "%s  did not FAIL", TEST_DESC);
      MPITEST_message(MPITEST_NONFATAL, info_buf);
   }
   else {
      MPI_Error_class(ierr, &errorclass);
      if (errorclass != MPI_ERR_DIMS && errorclass != MPI_ERR_ARG && errorclass != MPI_ERR_OTHER) {
	 fail++;
	 sprintf(info_buf, "%s  returned %d, expected MPI_ERR_DIMS(%d) or MPI_ERR_ARG(%d) or MPI_ERR_OTHER(%d)", TEST_DESC, errorclass, MPI_ERR_DIMS, MPI_ERR_ARG, MPI_ERR_OTHER);
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

   /* report overall results  */

   MPITEST_report(pass, fail, 0, testname);

   MPI_Finalize();

   return fail;

}				/* main() */
