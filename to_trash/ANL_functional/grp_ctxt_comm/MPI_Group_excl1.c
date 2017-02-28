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
			  Test for MPI_Group_excl

This test verifies that MPI_Group_excl creates a correct group from an
arbitrary commuicator.  This test creates a group consisting of the first
and last rank of MPI_COMM_WORLD.  This test also verifies the correct
operation when ranks=all ranks and ranks=0.

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
	 verify,        /* Counts number of tests to verify*/
	 ierr,		/* Return value from MPI calls     */
	*ranks,         /* Array of ranks in group         */
	 worldsize,
	 worldrank,
	 newsize,
	 result,
	 expect,
	 size,
	 i;

    MPI_Group	group,
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


    sprintf(testname, "MPI_Group_excl1");

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
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }


    ierr = MPI_Comm_size(MPI_COMM_WORLD, &worldsize);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_size #1", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }


    /* Create a group from the old group consisting of the last & first rank */
    ranks = (int *)malloc((worldsize+1) * sizeof(int));
    if (ranks == 0)
	MPITEST_message(MPITEST_FATAL, "Not enough memory for test buffers");

    if (worldsize == 1)
	newsize = 1;
    else
	newsize = 2;
    for (i=0; i< worldsize-1; i++)
	ranks[i] = i + 1;

    if (worldsize > 2)
	ierr = MPI_Group_excl(groupworld, worldsize - 2, ranks, &group);
    else
	ierr = MPI_Group_excl(groupworld, 0, ranks, &group);

    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_excl #1", ierr);
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
	expect = 0;
    else if (worldrank == worldsize - 1) 
	expect = 1;
    else expect = MPI_UNDEFINED;
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
    if (( (worldsize <= 2) && (result != MPI_IDENT) ) ||
	( (worldsize > 2)  && (result != MPI_UNEQUAL) ))
    {
	sprintf(info_buf, "MPI_Group_compare returned incorrect value %d", result);
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
	    MPITEST_message(MPITEST_NONFATAL, error_string);
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
	    MPITEST_message(MPITEST_NONFATAL, error_string);
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




    /* Free the new group */
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






    /* Now try removing all the ranks */
    for (i=0; i< worldsize; i++)
	ranks[i] = worldsize - 1 - i;
    ierr = MPI_Group_excl(groupworld, worldsize, ranks, &group);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_excl #2", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */
    loop_cnt++;

    if (group != MPI_GROUP_EMPTY)
    {
	sprintf(info_buf, "MPI_Group_excl of size 0 did not return MPI_GROUP_EMPTY)");
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	fail++;
    }
    

    /* Check the size of the new group */
    ierr = MPI_Group_size(group, &result);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_size(empty)", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */
    loop_cnt++;

    if (result != 0)
    {
	sprintf(info_buf, "MPI_Group_size(empty) returned %d, expected %d", result, 0);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	fail++;
    }
    loop_cnt++;



    /* Check my rank in the new group */
    ierr = MPI_Group_rank(group, &result);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_rank(empty)", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */
    loop_cnt++;
    if (result != MPI_UNDEFINED)
    {
	sprintf(info_buf, "MPI_Group_rank(empty) returned %d, expected MPI_UNDEFINED", result);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	fail++;
    }
    loop_cnt++;






    /* Now try removing no ranks */
    ierr = MPI_Group_excl(groupworld, 0, ranks, &group);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_excl #3", ierr);
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
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_size(all)", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */
    loop_cnt++;

    if (result != worldsize)
    {
	sprintf(info_buf, "MPI_Group_size(all) returned %d, expected %d", result, newsize);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	fail++;
    }
    loop_cnt++;



    /* Check my rank in the new group */
    ierr = MPI_Group_rank(group, &result);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_rank(empty)", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */
    loop_cnt++;
    if (result != worldrank)
    {
	sprintf(info_buf, "MPI_Group_rank(all) returned %d, expected %d", result, worldrank);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	fail++;
    }
    loop_cnt++;



    /* Compare the two groups */
    ierr = MPI_Group_compare(group, groupworld, &result);
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






    /* Free the earlier group */
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


    free(ranks);

    /* report overall results  */

    MPITEST_report(loop_cnt - fail, fail, verify, testname);

    MPI_Finalize();

    return fail;

}/* main() */
