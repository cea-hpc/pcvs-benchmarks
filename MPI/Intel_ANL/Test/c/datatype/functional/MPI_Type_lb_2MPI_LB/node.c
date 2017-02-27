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
                          Test for MPI_Type_lb()

Using MPI_Type_struct(), all rank will create a types from merging two
user defined types having MPI_UB.  All ranks then use call MPI_Type_lb() using
the newly created datatype and verify that the returned displacement is equal
to MPITEST_MIN_DISPL.

This test may be run in any communicator with a minimum of 1 group members,
with any data type, and with any message length.

The MPITEST environment provides looping over communicator size,
message length.  The properties of the loops are encoded in configuration
arrays in the file mpitest_cfg.h .

MPI Calls dependencies for this test:
  MPI_Init(), MPI_Finalize(), MPI_Type_lb(),
  MPI_Comm_test_inter(), MPI_Error_string(),
  MPI_Type_struct(), MPI_Type_commit(),
  MPI_Type_size(), MPI_Type_free(),
  [MPI_Get_count(), MPI_Allreduce(), MPI_Comm_rank(), MPI_Comm_size()]

Test history:
   1  07/08/96     simont       Original version

******************************************************************************/

#include "mpitest_cfg.h"
#include "mpitest.h"

/* Minimum displacement */
/* 11-8-02: Change this displacement to make it smaller than any other
   MPI_LB explicitly set in by the MPITEST_get_datatypes routine */
#define MPITEST_MIN_DISPL     13

int main(int argc, char *argv[])
{
   int
     test_type,         /*  the index of the current buffer type              */
     length_count,      /*  loop counter for length loop                      */
     length,            /*  The length of the current buffer                  */
     test_nump,         /*  The number of processors in current communicator  */
     comm_index,        /*  the array index of the current comm               */
     comm_type,         /*  the index of the current communicator type        */
     type_count,        /*  loop counter for data type loop                   */
     comm_count,        /*  loop counter for communicator loop                */
     fail,              /*  counts total number of failures                   */
     size,              /*  return size from MPI_Error_string                 */
     loop_cnt,          /*  counts total number of loops through test         */
     ierr;              /*  return value from MPI calls                       */

   char
     info_buf[256],     /*  buffer for passing mesages to MPITEST             */
     testname[128];     /*  the name of this test                             */
    char error_string[MPI_MAX_ERROR_STRING];

   MPI_Comm comm;       /*  MPI communicator                                  */

   MPI_Datatype type1, type2, newtype,
                *types1, *types2;

   int *blklens1, *blklens2, num_types, *type_sizes1, *type_sizes2;

   MPI_Aint xt, *displs1, *displs2, displ, tmp_lb, type1_lb = 0, type2_lb = 0;

   int inter_flag, type1_lb_flag, type2_lb_flag;

   ierr = MPI_Init(&argc, &argv);
   if (ierr!=MPI_SUCCESS) {
      sprintf(info_buf, "MPI_Init() returned %d", ierr);
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

   sprintf(testname, "MPI_Type_lb_2MPI_LB");

   MPITEST_init(argc, argv);

   if (MPITEST_me==0) {
      sprintf(info_buf, "Starting %s test", testname);
      MPITEST_message(MPITEST_INFO0, info_buf);
   }

   /* set the global error counter */
   fail = 0;
   loop_cnt = 0;

   num_types = MPITEST_num_datatypes();

   if (num_types == 0)
      MPITEST_message(MPITEST_FATAL, "No basic data types configured");

   /* for MPI_LB */
   num_types++;

   /* Set up various arrays */
   types1 = (MPI_Datatype *) calloc(num_types, sizeof(MPI_Datatype));
   if (!types1) {
      sprintf(info_buf, "Cannot allocate enough memory for types array");
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

   types2 = (MPI_Datatype *) calloc(num_types, sizeof(MPI_Datatype));
   if (!types2) {
      sprintf(info_buf, "Cannot allocate enough memory for types array");
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

   blklens1 = (int *) calloc(num_types, sizeof(int));
   if (!blklens1) {
      sprintf(info_buf, "Cannot allocate enough memory for blklens array");
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

   blklens2 = (int *) calloc(num_types, sizeof(int));
   if (!blklens2) {
      sprintf(info_buf, "Cannot allocate enough memory for blklens array");
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

   type_sizes1 = (int *) calloc(num_types, sizeof(int));
   if (!type_sizes1) {
      sprintf(info_buf, "Cannot allocate enough memory for type_sizes array");
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

   type_sizes2 = (int *) calloc(num_types, sizeof(int));
   if (!type_sizes2) {
      sprintf(info_buf, "Cannot allocate enough memory for type_sizes array");
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

   displs1 = (MPI_Aint *) calloc(num_types, sizeof(MPI_Aint));
   if (!displs1) {
      sprintf(info_buf, "Cannot allocate enough memory for displs array");
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

   displs2 = (MPI_Aint *) calloc(num_types, sizeof(MPI_Aint));
   if (!displs2) {
      sprintf(info_buf, "Cannot allocate enough memory for displs array");
      MPITEST_message(MPITEST_FATAL, info_buf);
   }

   for (comm_count=0; comm_count<MPITEST_num_comm_sizes();comm_count++) {
      comm_index = MPITEST_get_comm_index(comm_count);
      comm_type = MPITEST_get_comm_type(comm_count);

      test_nump = MPITEST_get_communicator(comm_type, comm_index, &comm);

      if (comm != MPI_COMM_NULL) {
	 if (test_nump < 1) {
	    /* Skipping communicator with comm size < 1 */
	    MPITEST_free_communicator(comm_type, &comm);
	    sprintf(info_buf, "Skipping communicator with comm_size < 1 (commtype: %d) for this test", comm_type);
	    MPITEST_message(MPITEST_INFO1, info_buf);
	    continue;
	 }
	 
	 ierr = MPI_Comm_test_inter(comm, &inter_flag);
	 if (ierr != MPI_SUCCESS) {
	    fail++;
	    sprintf(info_buf, "MPI_Comm_test_inter() returned %d", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_FATAL, error_string);
	 }
	 
	 if (inter_flag) {
	    /* Ignore inter-communicator for test */
	    sprintf(info_buf, "Skipping inter communicator (commtype: %d) for this test", comm_type);
	    MPITEST_message(MPITEST_INFO1, info_buf);
	    continue;
	 }

	 for (length_count=0;length_count<MPITEST_num_message_lengths();length_count++) { 
	    type1_lb_flag = type2_lb_flag = 0;
	    length = MPITEST_get_message_length(length_count);

	    for (type_count = 0; type_count < num_types - 1; type_count++) {
	       test_type = MPITEST_get_datatype(type_count);
	       types1[type_count] = MPITEST_mpi_datatypes[test_type];
	       types2[type_count] = MPITEST_mpi_datatypes[test_type];
	       
	       if (type_count == 0) {
		  displs1[type_count] = 0;
		  displs2[type_count] = 0;
	       }
	       else {
		  displs1[type_count] = displs1[type_count - 1] + 
			type_sizes1[type_count - 1];
		  displs2[type_count] = displs2[type_count - 1] + 
			type_sizes2[type_count - 1];
	       }
	       
	       blklens1[type_count] = 1;
	       blklens2[type_count] = 1;
	       
	       ierr = MPI_Type_size(types1[type_count], &(type_sizes1[type_count]));
	       if (ierr != MPI_SUCCESS) {
		  sprintf(info_buf, "MPI_Type_size() returned %d", ierr);
		  MPITEST_message(MPITEST_NONFATAL, info_buf);
		  MPI_Error_string(ierr, error_string, &size);
		  MPITEST_message(MPITEST_FATAL, error_string);
	       }
	       
	       ierr = MPI_Type_size(types2[type_count], &(type_sizes2[type_count]));
	       if (ierr != MPI_SUCCESS) {
		  sprintf(info_buf, "MPI_Type_size() returned %d", ierr);
		  MPITEST_message(MPITEST_NONFATAL, info_buf);
		  MPI_Error_string(ierr, error_string, &size);
		  MPITEST_message(MPITEST_FATAL, error_string);
	       }

	       /* check if components of types have explicit (sticky) lbs; record smallest */
	       ierr = MPITEST_datatype_has_sticky_lb(types1[type_count], &tmp_lb);
	       if (ierr == 1) {
		  /* this type has an explicit (sticky) lb already; see if it is smallest */
		  if (!type1_lb_flag || (tmp_lb + displs1[type_count] < type1_lb)) {
		     /* save smallest effective lb */
		     type1_lb_flag = 1;
		     type1_lb = tmp_lb + displs1[type_count];
		  }
	       }
	       ierr = MPITEST_datatype_has_sticky_lb(types2[type_count], &tmp_lb);
	       if (ierr == 1) {
		  if (!type2_lb_flag || (tmp_lb + displs2[type_count] < type2_lb)) {
		     type2_lb_flag = 1;
		     type2_lb = tmp_lb + displs2[type_count];
		  }
	       }
	    }
	    
	    if (type1_lb_flag) {
		sprintf(info_buf, "type1 has sticky lb at " MPITEST_AINT_FMT_DEC_SPEC, type1_lb);
		MPITEST_message(MPITEST_INFO1, info_buf);
	    }

	    if (type2_lb_flag) {
		sprintf(info_buf, "type2 has sticky lb at " MPITEST_AINT_FMT_DEC_SPEC, type2_lb);
		MPITEST_message(MPITEST_INFO1, info_buf);
	    }
	    
	    types1[num_types - 1] = MPI_LB;
	    displs1[num_types - 1] = MPITEST_MIN_DISPL * 2;
	    blklens1[num_types - 1] = 1;
	    type_sizes1[num_types - 1] = 0;

	    types2[num_types - 1] = MPI_LB;
	    displs2[num_types - 1] = MPITEST_MIN_DISPL;
	    blklens2[num_types - 1] = 1;
	    type_sizes2[num_types - 1] = 0;

	    /* save effective lbs for both types */
	    if (!type1_lb_flag || (displs1[num_types - 1] < type1_lb)) {
	       type1_lb = displs1[num_types - 1];
	    }

	    if (!type2_lb_flag || (displs2[num_types - 1] < type2_lb)) {
		type2_lb = displs2[num_types - 1];
	    }

	    ierr = MPI_Type_struct(num_types, blklens1, displs1, types1, &type1);
	    if (ierr != MPI_SUCCESS) {
	       sprintf(info_buf, "MPI_Type_struct() returned %d", ierr);
	       MPITEST_message(MPITEST_NONFATAL, info_buf);
	       MPI_Error_string(ierr, error_string, &size);
	       MPITEST_message(MPITEST_FATAL, error_string);
	    }
	    
	    ierr = MPI_Type_struct(num_types, blklens2, displs2, types2, &type2);
	    if (ierr != MPI_SUCCESS) {
	       sprintf(info_buf, "MPI_Type_struct() returned %d", ierr);
	       MPITEST_message(MPITEST_NONFATAL, info_buf);
	       MPI_Error_string(ierr, error_string, &size);
	       MPITEST_message(MPITEST_FATAL, error_string);
	    }
	    
	    /* Committing newly created datatype */
	    ierr = MPI_Type_commit(&type1);
	    if (ierr != MPI_SUCCESS) {
	       fail++;
	       sprintf(info_buf, "MPI_Type_commit() returned %d", ierr);
	       MPITEST_message(MPITEST_NONFATAL, info_buf);
	       MPI_Error_string(ierr, error_string, &size);
	       MPITEST_message(MPITEST_FATAL, error_string);
	    }
	    
	    ierr = MPI_Type_commit(&type2);
	    if (ierr != MPI_SUCCESS) {
	       fail++;
	       sprintf(info_buf, "MPI_Type_commit() returned %d", ierr);
	       MPITEST_message(MPITEST_NONFATAL, info_buf);
	       MPI_Error_string(ierr, error_string, &size);
	       MPITEST_message(MPITEST_FATAL, error_string);
	    }
	    
	    if (MPITEST_current_rank==0) {
	       sprintf(info_buf, "(%d, %d) length %d commsize %d commtype %d",
		       length_count, comm_count, length, test_nump, comm_type);
	       MPITEST_message(MPITEST_INFO1, info_buf);
	       
	       for (type_count = 0; type_count < num_types; type_count++) {
		  sprintf(info_buf, "blklens1[%d] = %d, displs1[%d] = " MPITEST_AINT_FMT_DEC_SPEC ", "
			  "types1[%d] = %d, type_sizes1[%d] = %d",
			  type_count, blklens1[type_count],
			  type_count, displs1[type_count],
			  type_count, types1[type_count],
			  type_count, type_sizes1[type_count]);
		  MPITEST_message(MPITEST_INFO2, info_buf);
	       }
	       for (type_count = 0; type_count < num_types; type_count++) {
		  sprintf(info_buf, "blklens2[%d] = %d, displs2[%d] = " MPITEST_AINT_FMT_DEC_SPEC ", "
			  "types2[%d] = %d, type_sizes2[%d] = %d",
			  type_count, blklens2[type_count],
			  type_count, displs2[type_count],
			  type_count, types2[type_count],
			  type_count, type_sizes2[type_count]);
		  MPITEST_message(MPITEST_INFO2, info_buf);
	       }
	    }
	    
	    /* Test MPI_Type_lb() for type1 */
	    ierr = MPI_Type_lb(type1, &displ);
	    if (ierr != MPI_SUCCESS) {
	       fail++;
	       sprintf(info_buf, "MPI_Type_lb() returned %d", ierr);
	       MPITEST_message(MPITEST_NONFATAL, info_buf);
	       MPI_Error_string(ierr, error_string, &size);
	       MPITEST_message(MPITEST_FATAL, error_string);
	    }
	    else if (displ != type1_lb) {
	       fail++;
	       sprintf(info_buf, "MPI_Type_lb() returned unexpected displacement (type1) Expected: " MPITEST_AINT_FMT_DEC_SPEC ", Actual: " MPITEST_AINT_FMT_DEC_SPEC,
		       type1_lb, displ);
	       MPITEST_message(MPITEST_NONFATAL, info_buf);
	    }

	    /* Test MPI_Type_lb() for type2 */
	    ierr = MPI_Type_lb(type2, &displ);
	    if (ierr != MPI_SUCCESS) {
	       fail++;
	       sprintf(info_buf, "MPI_Type_lb() returned %d", ierr);
	       MPITEST_message(MPITEST_NONFATAL, info_buf);
	       MPI_Error_string(ierr, error_string, &size);
	       MPITEST_message(MPITEST_FATAL, error_string);
	    }
	    else if (displ != type2_lb) {
	       fail++;
	       sprintf(info_buf, "MPI_Type_lb() returned unexpected displacement (type2) Expected: " MPITEST_AINT_FMT_DEC_SPEC ", Actual: " MPITEST_AINT_FMT_DEC_SPEC,
		       type2_lb, displ);
	       MPITEST_message(MPITEST_NONFATAL, info_buf);
	    }

	    /* Merging the 2 user created datatypes */
	    blklens1[0] = 1;
	    blklens1[1] = 1;
	    displs1[0] = length;
	    
            ierr = MPI_Type_extent(type1, &xt);
            if (ierr != MPI_SUCCESS) {
               sprintf(info_buf, "MPI_Type_extent() returned %d", ierr);
               MPITEST_message(MPITEST_NONFATAL, info_buf);
               MPI_Error_string(ierr, error_string, &size);
               MPITEST_message(MPITEST_FATAL, error_string);
            }

	    ierr = MPI_Type_size(type1, &size);
	    if (ierr != MPI_SUCCESS) {
	       fail++;
	       sprintf(info_buf, "MPI_Type_size() returned %d", ierr);
	       MPITEST_message(MPITEST_NONFATAL, info_buf);
	       MPI_Error_string(ierr, error_string, &size);
	       MPITEST_message(MPITEST_FATAL, error_string);
	    }
	    
	    displs1[1] = displs1[0] + xt;
	    
	    types1[0] = type1;
	    types1[1] = type2;
	    
	    ierr = MPI_Type_struct(2, blklens1, displs1, types1, &newtype);
	    if (ierr != MPI_SUCCESS) {
	       sprintf(info_buf, "MPI_Type_struct() returned %d", ierr);
	       MPITEST_message(MPITEST_NONFATAL, info_buf);
	       MPI_Error_string(ierr, error_string, &size);
	       MPITEST_message(MPITEST_FATAL, error_string);
	    }
	    
	    /* Committing newly created datatype */
	    ierr = MPI_Type_commit(&newtype);
	    if (ierr != MPI_SUCCESS) {
	       fail++;
	       sprintf(info_buf, "MPI_Type_commit() returned %d", ierr);
	       MPITEST_message(MPITEST_NONFATAL, info_buf);
	       MPI_Error_string(ierr, error_string, &size);
	       MPITEST_message(MPITEST_FATAL, error_string);
	    }

	    loop_cnt++;

	    /* Calculate expected lb for this type (this was broken in older versions of this test) */
	    if (displs1[0] + type1_lb < displs1[1] + type2_lb) {
	       tmp_lb = displs1[0] + type1_lb;
	    }
	    else {
	       tmp_lb = displs1[1] + type2_lb;
	    }

	    /* Test MPI_Type_lb() for this datatype */
	    ierr = MPI_Type_lb(newtype, &displ);
	    if (ierr != MPI_SUCCESS) {
	       fail++;
	       sprintf(info_buf, "MPI_Type_lb() returned %d", ierr);
	       MPITEST_message(MPITEST_NONFATAL, info_buf);
	       MPI_Error_string(ierr, error_string, &size);
	       MPITEST_message(MPITEST_FATAL, error_string);
	    }
	    else if (displ != tmp_lb) {
	       fail++;
	       sprintf(info_buf, "MPI_Type_lb() returned unexpected displacement  Expected: " MPITEST_AINT_FMT_DEC_SPEC ", Actual: " MPITEST_AINT_FMT_DEC_SPEC, tmp_lb, displ);
	       MPITEST_message(MPITEST_NONFATAL, info_buf);
	    }

	    /* Free newly created datatype */
	    ierr = MPI_Type_free(&newtype);
	    if (ierr != MPI_SUCCESS) {
	       fail++;
	       sprintf(info_buf, "MPI_Type_free() returned %d", ierr);
	       MPITEST_message(MPITEST_NONFATAL, info_buf);
	       MPI_Error_string(ierr, error_string, &size);
	       MPITEST_message(MPITEST_FATAL, error_string);
	    }

	    ierr = MPI_Type_free(&type1);
	    if (ierr != MPI_SUCCESS) {
	       fail++;
	       sprintf(info_buf, "MPI_Type_free() returned %d", ierr);
	       MPITEST_message(MPITEST_NONFATAL, info_buf);
	       MPI_Error_string(ierr, error_string, &size);
	       MPITEST_message(MPITEST_FATAL, error_string);
	    }

	    ierr = MPI_Type_free(&type2);
	    if (ierr != MPI_SUCCESS) {
	       fail++;
	       sprintf(info_buf, "MPI_Type_free() returned %d", ierr);
	       MPITEST_message(MPITEST_NONFATAL, info_buf);
	       MPI_Error_string(ierr, error_string, &size);
	       MPITEST_message(MPITEST_FATAL, error_string);
	    }

#ifdef MPITEST_SYNC
	    ierr = MPI_Barrier(comm);
	    if (ierr != MPI_SUCCESS) {
	       sprintf(info_buf, "MPI_Barrier() returned %d", ierr);
	       MPITEST_message(MPITEST_NONFATAL, info_buf);
	       MPI_Error_string(ierr, error_string, &size);
	       MPITEST_message(MPITEST_FATAL, error_string);
	    }
#endif
	 }
      }

      MPITEST_free_communicator(comm_type, &comm);
   }
   free(types1);
   free(blklens1);
   free(displs1);

   free(types2);
   free(blklens2);
   free(displs2);

   /* report overall results  */
   MPITEST_report(loop_cnt-fail, fail, 0, testname);

   ierr = MPI_Finalize();
   if (ierr != MPI_SUCCESS) {
      fail++;
      sprintf(info_buf, "MPI_Finalize() returned %d, FAILED", ierr);
      MPITEST_message(MPITEST_NONFATAL, info_buf);
      MPI_Error_string(ierr, error_string, &size);
      MPITEST_message(MPITEST_FATAL, error_string);
   }

   return 0;
} /* main() */

