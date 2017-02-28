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
			  Test for MPI_Group_intersection

This test verifies that MPI_Group_intersection works correctly with
overlapping groups created from a subset of MPI_COMM_WORLD in both
directions.

group1 = all ranks EXCEPT 0, 1 and the last, in reverse order.
group2 = ranks 2, 0, then every other rank, starting at rank 4, excluding
	 the last rank if size is odd.

which would contain the following ranks in MPI_COMM_WORLD:
# ranks:     4       5        6           7             8             9
group1       2      3,2     4,3,2      5,4,3,2      6,5,4,3,2    7,6,5,4,3,2
group2      2,0     2,0     2,0,4       2,0,4        2,0,4,6       2,0,4,6

What should be an identical group is compared to that created by
MPI_Group_intersection.

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
	 ranks[3][3],	/* To create test groups           */
	 result,
	 expect,
	 size;

    MPI_Group group,      /* Result of intersection of group[12]   */
	      groupmine,  /* Goup which should match intersection  */
	      group1,
	      group2,
	      groupworld;

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


    sprintf(testname, "MPI_Group_intersection3");

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

    if (MPITEST_nump < 4)
	MPITEST_message(MPITEST_FATAL, "This test requires at least 4 ranks");

    /* Get a group for the MPI_COMM_WORLD */
    ierr = MPI_Comm_group(MPI_COMM_WORLD, &groupworld);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_group", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_FATAL, error_string);
	fail++;
    }       /* Error Test  */
    loop_cnt++;


    /* Create the 2 groups */
    ranks[0][0] = MPITEST_nump - 2;
    ranks[0][1] = 2;
    ranks[0][2] = -1;
    ierr = MPI_Group_range_incl(groupworld, 1, ranks, &group1);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_excl", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_FATAL, error_string);
	fail++;
    }       /* Error Test  */

    ranks[0][0] = 2;
    ranks[0][1] = 0;
    ranks[0][2] = -2;
    ranks[1][0] = 4;
    ranks[1][1] = MPITEST_nump-1;
    if (MPITEST_nump %2 == 1)
	ranks[1][1] = ranks[1][1] - 1; 
    ranks[1][2] = 2;
    if (MPITEST_nump < 6) 
	ierr = MPI_Group_range_incl(groupworld, 1, ranks, &group2);
    else
	ierr = MPI_Group_range_incl(groupworld, 2, ranks, &group2);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_incl", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_FATAL, error_string);
	fail++;
    }   /* Error Test  */



    ierr = MPI_Group_intersection (group1, group2, &group);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_intersection(group1, group2)", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */
    loop_cnt++;


    ranks[0][0] = MPITEST_nump - MPITEST_nump%2 - 2;
    ranks[0][1] = 2;
    ranks[0][2] = -2;
    ierr = MPI_Group_range_incl(groupworld, 1, ranks, &groupmine);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_range_incl(groupmine)", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */
    loop_cnt++;

    ierr = MPI_Group_compare(groupmine, group, &result);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_compare(group1, group2)", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */
    loop_cnt++;
    if (result != MPI_IDENT)
    {
	fail++;
	sprintf(info_buf, "MPI_Group_compare(group1, group2) returned %d, expected MPI_IDENT", result);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
    }
    loop_cnt++;

    /* Check my rank in the new group */
    ierr = MPI_Group_rank(group, &result);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_rank(group1, group2)", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */
    loop_cnt++;

    /* Calculate the expected rank and verify */
    if ((MPITEST_me <= 1) ||
	(MPITEST_me == MPITEST_nump -1) ||
	(MPITEST_me%2 == 1))
	expect = MPI_UNDEFINED;
    else
	expect = MPITEST_nump/2 - MPITEST_me/2 - 1;
    if (result != expect)
    {
	fail++;
	sprintf(info_buf, "MPI_Group_rank(group1, group2) returned %d, expected %d", result, expect);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
    }
    loop_cnt++;

    ierr = MPI_Group_free(&group);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_free(group)", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */
    loop_cnt++;

    if (groupmine != MPI_GROUP_EMPTY)
    {
        ierr = MPI_Group_free(&groupmine);
        if (ierr != MPI_SUCCESS)
        {
	    sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_free(groupmine)", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_NONFATAL, error_string);
	    fail++;
        }       /* Error Test  */
    loop_cnt++;

    }


    /* Now try with the reverse intersection */
    ierr = MPI_Group_intersection (group2, group1, &group);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_intersection(group2, group1)", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */
    loop_cnt++;

    ranks[0][0] = 2;
    ranks[0][1] = MPITEST_nump - 2;
    ranks[0][2] = 2;

    ierr = MPI_Group_range_incl(groupworld, 1, ranks, &groupmine);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_incl", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_FATAL, error_string);
	fail++;
    }   /* Error Test  */

    ierr = MPI_Group_compare(group, groupmine, &result);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_compare(group2, group1)", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */
    loop_cnt++;
    if (result != MPI_IDENT)
    {
	fail++;
	sprintf(info_buf, "MPI_Group_compare(group2, group1) returned %d, expected MPI_IDENT", result);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
    }
    loop_cnt++;

    /* Check my rank in the new group */
    ierr = MPI_Group_rank(group, &result);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_rank(group2, group1)", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */
    loop_cnt++;

    /* Calculate the expected rank and verify */
    if ((MPITEST_me <= 1) ||
	(MPITEST_me == MPITEST_nump -1) ||
	(MPITEST_me%2 == 1))
	expect = MPI_UNDEFINED;
    else
	expect = MPITEST_me/2 - 1;
    if (result != expect)
    {
	fail++;
	sprintf(info_buf, "MPI_Group_rank(group2, group1) returned %d, expected %d", result, expect);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
    }
    loop_cnt++;

    ierr = MPI_Group_free(&group);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_free(group)", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */
    loop_cnt++;

    ierr = MPI_Group_free(&groupmine);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_free(groupmine)", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */
    loop_cnt++;


    /* Free the groups */
    ierr = MPI_Group_free(&groupworld);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_free(groupworld)", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */
    loop_cnt++;

    ierr = MPI_Group_free(&group1);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_free(group1)", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */
    loop_cnt++;

    ierr = MPI_Group_free(&group2);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_free(group2)", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */
    loop_cnt++;



    /* report overall results  */

    MPITEST_report(loop_cnt - fail, fail, verify, testname);

    MPI_Finalize();

    return fail;

}/* main() */
