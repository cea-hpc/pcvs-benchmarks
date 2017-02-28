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
		  Test for MPI_Group_compare

This test verifies that MPI_Group_compare compares correctly
against another group from an arbitrary communicator.  Each configured
communicator is tested.

The following comparisons are made:
 1 - group against group w/ all ranks reversed
 2 - same but reversed
 3 - group against itself.
 4 - group against MPI_GROUP_EMPTY
 5 - same but reversed
 6 - group missing one rank
 7 - same but reversed
 

As a consequence of this test  MPI_Group_incl and MPI_Group_free are also
tested here.

Test history:
   1  08/28/96     gt       Original version

******************************************************************************/

#include "mpitest_cfg.h"
#include "mpitest.h"


int main(int argc, char *argv[])
{
    int
	 fail,		/* Counts number of test failures  */
	 loop_cnt,	/* Counts number of tests executed */
	 verify,        /* Counts number of tests to verify*/
	 ierr,		/* Return value from MPI calls     */
	*ranks1,        /* Array of ranks in group         */
	 test_nump,     /* number of ranks in current comm */
	 comm_index,    /* array index of current comm     */
	 comm_type,     /* index of current comm type      */
	 comm_count,    /* number of communicators to test */
	 type_count,    /* loop counter for data type loop */
	 commsize,
	 result,
	 expect,
	 size,
	 i;

    MPI_Group	group1,
		group2,
		group3;

    MPI_Comm comm;      /* Communicator under test         */

    char
	  info_buf[256],/* buffer for passing mesages to MPITEST  */
	  testname[64]; /* the name of this test           */
    char error_string[MPI_MAX_ERROR_STRING];

    /*-----------------------------  MPI_Init  ------------------------------*/
    ierr = MPI_Init(&argc, &argv);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr);
	MPITEST_message(MPITEST_FATAL, info_buf);
    }


    sprintf(testname, "MPI_Group_compare");

    /*-----------------------------  MPITEST_init  --------------------------*/
    MPITEST_init(argc, argv);
    if (MPITEST_me == 0)
    {
	sprintf(info_buf, "Starting %s test", testname);
	MPITEST_message(MPITEST_INFO0, info_buf);
    }

    /* set the global error counter */
    fail = 0;
    verify = 0;
    loop_cnt = 0;

    /* Loop through the configured communicators */
    for (comm_count=0; comm_count<MPITEST_num_comm_sizes(); comm_count++)
    {
	comm_index = MPITEST_get_comm_index(comm_count);
	comm_type = MPITEST_get_comm_type(comm_count);

	test_nump = MPITEST_get_communicator(comm_type, comm_index, &comm);

	/* Only test if this node is part of the current communicator */
	if (MPITEST_current_rank != MPI_UNDEFINED)
	{
	    ranks1 = (int *)malloc((test_nump + 1) * sizeof(int));
	    if (ranks1 == 0)
		MPITEST_message(MPITEST_FATAL, "Not enough memory for test buffers");

	    ierr = MPI_Comm_group(comm, &group1);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_group (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;


	    /* Get the size of the new group */
	    ierr = MPI_Comm_size(comm, &commsize);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_size #2 (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }


	    /* Reverse the ranks */
	    for (i = 0; i < commsize; i++)
		ranks1[i] = commsize - 1 - i;
	    ierr = MPI_Group_incl(group1, commsize, ranks1, &group2);
	    if (ierr != MPI_SUCCESS)
	    {
	        sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_incl #1 (comm_index %d)", ierr, comm_index);
	        MPITEST_message(MPITEST_NONFATAL, info_buf);
	        MPI_Error_string(ierr, error_string, &size);
	        MPITEST_message(MPITEST_FATAL, error_string);
	        fail++;
	    }       /* Error Test  */
	    loop_cnt++;





	    /* Now compare the groups */
	    ierr = MPI_Group_compare(group1, group2, &result);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_compare #1 (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;

	    /* Check the result */
	    if (commsize == 1)
		expect = MPI_IDENT;
	    else 
		expect = MPI_SIMILAR;
	    if (result != expect)
	    {
		sprintf(info_buf, "MPI_Group_compare #1 returned %d, expected %d(comm_index %d)", result, expect, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;


	    ierr = MPI_Group_compare(group2, group1, &result);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_compare #2 (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    else if (result != expect)
	    {
		sprintf(info_buf, "MPI_Group_compare #2 returned %d, expected %d(comm_index %d)", result, expect, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;






	    /* Now a group to itself */
	    ierr = MPI_Group_compare(group1, group1, &result);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_compare #3 (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;

	    /* Check the result */
	    if (result != MPI_IDENT)
	    {
		sprintf(info_buf, "MPI_Group_compare #3 returned %d, expected MPI_IDENT (comm_index %d)", result, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;






	    /* Now a group to the empty group */
	    ierr = MPI_Group_compare(group1, MPI_GROUP_EMPTY, &result);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_compare #4 (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;

	    /* Check the result */
	    if (result != MPI_UNEQUAL)
	    {
		sprintf(info_buf, "MPI_Group_compare #4 returned %d, expected MPI_UNEQUAL (comm_index %d)", result, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;


	    ierr = MPI_Group_compare(MPI_GROUP_EMPTY, group2, &result);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_compare #5 (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;

	    /* Check the result */
	    if (result != MPI_UNEQUAL)
	    {
		sprintf(info_buf, "MPI_Group_compare #5 returned %d, expected MPI_UNEQUAL (comm_index %d)", result, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;






	    /* Finally, do a group less one rank */
	    ierr = MPI_Group_incl(group1, commsize-1, ranks1, &group3);
	    if (ierr != MPI_SUCCESS)
	    {
	        sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_incl #1", ierr);
	        MPITEST_message(MPITEST_NONFATAL, info_buf);
	        MPI_Error_string(ierr, error_string, &size);
	        MPITEST_message(MPITEST_FATAL, error_string);
	        fail++;
	    }       /* Error Test  */
	    loop_cnt++;
	    ierr = MPI_Group_compare(group1, group3, &result);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_compare #6 (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;

	    /* Check the result */
	    if (result != MPI_UNEQUAL)
	    {
		sprintf(info_buf, "MPI_Group_compare #6 returned %d, expected MPI_UNEQUAL (comm_index %d)", result, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;


	    ierr = MPI_Group_compare(group3, group2, &result);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_compare #7 (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;

	    /* Check the result */
	    if (result != MPI_UNEQUAL)
	    {
		sprintf(info_buf, "MPI_Group_compare #7 returned %d, expected MPI_UNEQUAL (comm_index %d)", result, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;







	    /* Free the groups */
	    ierr = MPI_Group_free(&group1);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_free #1 (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;

	    ierr = MPI_Group_free(&group2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_free #2 (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;

	    ierr = MPI_SUCCESS;
	    if (group3 != MPI_GROUP_EMPTY)
	        ierr = MPI_Group_free(&group3);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_free #3 (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;


	    MPITEST_free_communicator(comm_type, &comm);

	    free(ranks1);
	} /* Node is in this communicator */

    } /* Communicator loop */


    /* report overall results  */

    MPITEST_report(loop_cnt - fail, fail, 0, testname);

    MPI_Finalize();

    return fail;

}/* main() */
