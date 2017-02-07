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
			  Test for MPI_Comm_create

This test verifies that MPI_Comm_create creates a correct communicator from an
arbitrary group.  Each configured communicator is tested.

As a consequence of this test, MPI_Group_free, MPI_Comm_size, MPI_Comm_rank,
MPI_Group_free and the non-propogation of attributes are tested here.

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
	 test_nump,     /* number of ranks in current comm */
	 comm_index,    /* array index of current comm     */
	 comm_type,     /* index of current comm type      */
	 comm_count,    /* number of communicators to test */
	 type_count,    /* loop counter for data type loop */
	 intercomm,     /* result of MPI_Comm_test_inter   */
	 key,           /* MPI key                         */
	 found,         /* From MPI_Attr_get               */
	*attr,
	 commsize,
	 result,
	 size;

    MPI_Group	group;

    MPI_Comm comm,      /* Communicator under test         */
	     comm2;     /* For creating communicator       */

    MPI_Aint extra;     /* For keyval creation/use         */

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


    sprintf(testname, "MPI_Comm_create");

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


    /* Create a keyval */
    ierr = MPI_Keyval_create(MPI_DUP_FN, MPI_NULL_DELETE_FN, &key, &extra);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Keyval_create", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */



    /* Loop through the configured communicators */
    for (comm_count=0; comm_count<MPITEST_num_comm_sizes(); comm_count++)
    {
	comm_index = MPITEST_get_comm_index(comm_count);
	comm_type = MPITEST_get_comm_type(comm_count);

	test_nump = MPITEST_get_communicator(comm_type, comm_index, &comm);

	/* Only test if this node is part of the current communicator */
	if (MPITEST_current_rank != MPI_UNDEFINED)
	{

	    /* Set a value for the attribute */
	    result = 0;
	    ierr = MPI_Attr_put(comm, key, &result);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_put (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;



	    /* Create a new group */
	    ierr = MPI_Comm_group(comm, &group);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_group (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;


	    /* Check for intercommunicator */
	    ierr = MPI_Comm_test_inter(comm, &intercomm);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_test_inter (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;


/*	    if (intercomm) printf( "intercomm\n" ); */
	    if (intercomm == 0)
	    {	/* Can only create a communicator from an intracommunicator */
		/* Create a new communicator with the group */
/*		{
		    int gsize, csize;
		    MPI_Comm_size( comm, &csize );
		    MPI_Group_size( group, &gsize );
		    printf( "csize = %d gsize = %d\n", csize, gsize );
		    } */
		ierr = MPI_Comm_create(comm, group, &comm2);
		if (ierr != MPI_SUCCESS)
		{
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_create (comm_index %d)", ierr, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_FATAL, error_string);
		    fail++;
		}       /* Error Test  */
		loop_cnt++;




		/* Check my rank in the new comm */
		ierr = MPI_Comm_rank(comm2, &result);
		if (ierr != MPI_SUCCESS)
		{
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_rank (comm_index %d)", ierr, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_NONFATAL, error_string);
		    fail++;
		}       /* Error Test  */
		else if (result != MPITEST_current_rank)
		{
		    sprintf(info_buf, "MPI_Comm_rank returned %d, expected %d (comm_index %d)", result, MPITEST_current_rank, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    fail++;
		}
		loop_cnt++;



		/* Check the size of the new comm */
	        ierr = MPI_Comm_size(comm, &commsize);
	        if (ierr != MPI_SUCCESS)
	        {
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_size #1 (comm_index %d)", ierr, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_NONFATAL, error_string);
		    fail++;
	        }



		ierr = MPI_Comm_size(comm2, &result);
		if (ierr != MPI_SUCCESS)
		{
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_size #2 (comm_index %d)", ierr, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_NONFATAL, error_string);
		    fail++;
		}       /* Error Test  */
		loop_cnt++;

		if (result != commsize)
		{
		    sprintf(info_buf, "MPI_Comm_size returned %d, expected %d (comm_index %d)", result, commsize, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    fail++;
		}
		loop_cnt++;
    


		/* Get the value for the attribute; it should NOT be there */
		ierr = MPI_Attr_get(comm2, key, &attr, &found);
		if (ierr != MPI_SUCCESS)
		{
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_get", ierr);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_NONFATAL, error_string);
		    fail++;
		}       /* Error Test  */
		else if (found != 0)
		{
		    fail++;
		    sprintf(info_buf, "MPI_Attr_get found attribute in created comm; it should not (comm_index %d).", comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		}
		loop_cnt++;




		/* Try the new communicator */
		if (MPITEST_current_rank == 0)
		    result = 127;
		ierr = MPI_Bcast(&result, 1, MPI_INT, 0, comm2);
		if (ierr != MPI_SUCCESS)
		{
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_Bcast", ierr);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_NONFATAL, error_string);
		    fail++;
		}       /* Error Test  */
		else if (result != 127)
		{
		    fail++;
		    sprintf(info_buf, "MPI_Bcast received incorrect data %d, expected %d (comm_index %d).", result, 127, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		}
		loop_cnt++;


		ierr = MPI_Group_free(&group);
		if (ierr != MPI_SUCCESS)
		{
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_free (comm_index %d)", ierr, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_NONFATAL, error_string);
		    fail++;
		}       /* Error Test  */
		loop_cnt++;
		if (group != MPI_GROUP_NULL)
		{
		    sprintf(info_buf, "group = %d after MPI_Group_free, expected MPI_GROUP_NULL (comm_index %d)", group, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    fail++;
		}
		loop_cnt++;



		/* Try something again with the first group free */
		ierr = MPI_Barrier(comm2);
		if (ierr != MPI_SUCCESS)
		{
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_Barrier (comm_index %d)", ierr, comm_index);
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
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_free (comm_index %d)", ierr, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_NONFATAL, error_string);
		    fail++;
		}       /* Error Test  */
		loop_cnt++;
    
	    }
	    else /* Still need to free the group */
	    {
		ierr = MPI_Group_free(&group);
		if (ierr != MPI_SUCCESS)
		{
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_free (comm_index %d)", ierr, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_NONFATAL, error_string);
		    fail++;
		}       /* Error Test  */
		else if (group != MPI_GROUP_NULL)
		{
		    sprintf(info_buf, "group = %d after MPI_Group_free, expected MPI_GROUP_NULL (comm_index %d)", group, comm_index);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    fail++;
		}
		loop_cnt++;
	    }



	    /* Now delete the attribute */
	    ierr = MPI_Attr_delete(comm, key);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_delete (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */


	    MPITEST_free_communicator(comm_type, &comm);

	} /* Node is in this communicator */

    } /* Communicator loop */

    /* Free the Keyval */
    ierr = MPI_Keyval_free(&key);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Keyval_free", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */


    /* report overall results  */

    MPITEST_report(loop_cnt - fail, fail, verify, testname);

    MPI_Finalize();

    return fail;

}/* main() */
