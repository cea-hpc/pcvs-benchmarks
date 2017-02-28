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
			  Test for MPI_Group_range_excl

This test verifies that MPI_Group_range_excl creates a correct group from an
arbitrary commuicator.  This test creates a group consisting of all but
the first rank of MPI_COMM_WORLD.

As a consequence of this test, MPI_Group_size, MPI_Group_rank, MPI_Group_free,
MPI_Group_compare, MPI_Comm_create and MPI_Comm_free are also tested here.

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
	 verify,	/* Counts number of tests to verify*/
	 ierr,		/* Return value from MPI calls     */
	 worldsize,
	 worldrank,
	 range[1][3],
	 newsize,
	 result,
	 expect,
	 size;

    MPI_Group	group,
		group2,
		groupworld;

    MPI_Comm comm,      /* Communicator under test         */
	     comm2;     /* For creating communicator       */

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


    sprintf(testname, "MPI_Group_range_excl2");

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



    /* Get the size of the application & my rank within */
    ierr = MPI_Comm_rank(MPI_COMM_WORLD, &worldrank);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_rank #1", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_FATAL, error_string);
	fail++;
    }


    ierr = MPI_Comm_size(MPI_COMM_WORLD, &worldsize);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_size #1", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_FATAL, error_string);
	fail++;
    }

    if (worldsize == 1)
    {
	MPITEST_message(MPITEST_FATAL, "This test requires at least 2 ranks to run");
    }

    /* Create a group from the old group consisting of all but the first rank */
    newsize = worldsize - 1;

    range[0][0] = 0;
    range[0][1] = 0;
    range[0][2] = 1;
    ierr = MPI_Group_range_excl(groupworld, 1, range, &group);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_range_excl #1", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */
    loop_cnt++;


    /* Check the size of the new group */
    ierr = MPI_Group_size(group, &result);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_size", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */
    loop_cnt++;

    if (result != newsize)
    {
	sprintf(info_buf, "MPI_Group_size returned %d, expected %d", result, newsize);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	fail++;
    }
    loop_cnt++;



    /* Check my rank in the new group */
    ierr = MPI_Group_rank(group, &result);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_rank", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */
    loop_cnt++;
    if (worldrank == 0)
	expect = MPI_UNDEFINED;
    else
	expect = worldrank - 1;
    if (result != expect)
    {
	sprintf(info_buf, "MPI_Group_rank returned %d, expected %d", result, expect);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	fail++;
    }
    loop_cnt++;


    /* Compare the two groups */
    ierr = MPI_Group_compare(group, groupworld, &result);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_compare", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */

    if (result != MPI_UNEQUAL)
    {
	sprintf(info_buf, "MPI_Group_compare returned incorrect value %d, expected MPI_UNEQUAL", result);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	fail++;
    }
    loop_cnt++;



    /* Create the communicator */
    ierr = MPI_Comm_create(MPI_COMM_WORLD, group, &comm2);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_create", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */
    loop_cnt++;



    if (expect == MPI_UNDEFINED)
    {
	if (comm2 != MPI_COMM_NULL)
	{
	    sprintf(info_buf, "MPI_Comm_create did not return MPI_COMM_NULL to ranks not in new communicator");
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    fail++;
	}
    }
    else
    {
	/* Get the size of the new comm & my rank within */
	ierr = MPI_Comm_rank(comm2, &result);
	if (ierr != MPI_SUCCESS)
	{
	    sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_rank #2", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_FATAL, error_string);
	    fail++;
	}
	if (result != expect)
	{
	    sprintf(info_buf, "MPI_Comm_rank #2 returned %d, expected %d", result, expect);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    fail++;
	}
	loop_cnt++;


	ierr = MPI_Comm_size(comm2, &result);
	if (ierr != MPI_SUCCESS)
	{
	    sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_size #2", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_FATAL, error_string);
	    fail++;
	}
	if (result != newsize)
	{
	    sprintf(info_buf, "MPI_Comm_size #2 returned %d, expected %d", result, newsize);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    fail++;
	}
	loop_cnt++;

	/* Try the new communicator */
	if (expect == 0)
	    result = 29;
	ierr = MPI_Bcast(&result, 1, MPI_INT, 0, comm2);
	if (ierr != MPI_SUCCESS)
	{
	    sprintf(info_buf, "Non-zero return code (%d) from MPI_Bcast", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_NONFATAL, error_string);
	    fail++;
	}       /* Error Test  */
	else if (result != 29)
	{
	    fail++;
	    sprintf(info_buf, "MPI_Bcast received incorrect data %d, expected %d", result, 29);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	}
	loop_cnt++;


	ierr = MPI_Comm_group(comm2, &group2);
	if (ierr != MPI_SUCCESS)
	{
	    sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_group #2", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_FATAL, error_string);
	    fail++;
	}       /* Error Test  */
	loop_cnt++;


	/* Compare the two groups */
	ierr = MPI_Group_compare(group, group2, &result);
	if (ierr != MPI_SUCCESS)
	{
	    sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_compare #2", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_NONFATAL, error_string);
	    fail++;
	}       /* Error Test  */

	if (result != MPI_IDENT)
	{
	    sprintf(info_buf, "MPI_Group_compare #2 returned incorrect value %d, expected MPI_IDENT", result);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    fail++;
	}
	loop_cnt++;


	/* Free the new group */
	ierr = MPI_Group_free(&group2);
	if (ierr != MPI_SUCCESS)
	{
	    sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_free", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_NONFATAL, error_string);
	    fail++;
	}       /* Error Test  */
	loop_cnt++;



	/* Free the created communicator */
	ierr = MPI_Comm_free(&comm2);
	if (ierr != MPI_SUCCESS)
	{
	    sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_free", ierr);
	    MPITEST_message(MPITEST_NONFATAL, info_buf);
	    MPI_Error_string(ierr, error_string, &size);
	    MPITEST_message(MPITEST_NONFATAL, error_string);
	    fail++;
	}       /* Error Test  */
	loop_cnt++;
    }




    /* Free the group */
    ierr = MPI_Group_free(&group);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_free", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */
    loop_cnt++;
    if (group != MPI_GROUP_NULL)
    {
	sprintf(info_buf, "group = %d after MPI_Group_free, expected MPI_GROUP_NULL", group);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	fail++;
    }
    loop_cnt++;


    /* Free the other group */
    ierr = MPI_Group_free(&groupworld);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_free", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */
    loop_cnt++;
    if (groupworld != MPI_GROUP_NULL)
    {
	sprintf(info_buf, "groupworld = %d after MPI_Group_free, expected MPI_GROUP_NULL", groupworld);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	fail++;
    }
    loop_cnt++;


    /* report overall results  */

    MPITEST_report(loop_cnt - fail, fail, verify, testname);

    MPI_Finalize();

    return fail;

}/* main() */
