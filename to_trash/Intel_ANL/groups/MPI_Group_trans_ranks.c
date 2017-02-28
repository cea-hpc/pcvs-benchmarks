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
		  Test for MPI_Group_translate_ranks

This test verifies that MPI_Group_translate_ranks translates correctly 
against another group from an arbitrary commuicator.  Each configured
communicator is tested.

The following comparisons are made:
 1 - group against group w/ all ranks reversed
 2 - group w/ all ranks reversed against group
 3 - same as #2, but only one rank
 4 - same as #1, but only one rank
 5 - self against group w/ all ranks reversed
 6 - group against MPI_GROUP_EMPTY 
 7 - group against self (all ranks)
 8 - group against itself.

As a consequence of this test, MPI_Group_size, MPI_Group_rank, MPI_Group_incl
and MPI_Group_free are also tested here.

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
	*ranks2,        /* Array of ranks in group         */
	 test_nump,     /* number of ranks in current comm */
	 comm_index,    /* array index of current comm     */
	 comm_type,     /* index of current comm type      */
	 comm_count,    /* number of communicators to test */
	 type_count,    /* loop counter for data type loop */
	 commsize,
	 result,
	 size,
	 i;

    MPI_Group	group,
		group2,
		groupself;

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


    sprintf(testname, "MPI_Group_translate_ranks");

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
	    ranks2 = (int *)malloc((test_nump + 1) * sizeof(int));
	    if ((ranks1 == 0) || (ranks2 == 0))
		MPITEST_message(MPITEST_FATAL, "Not enough memory for test buffers");

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


	    /* Get the size of the new group */
	    ierr = MPI_Comm_size(comm, &commsize);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_size #2 (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }


	    /* Reverse the ranks */
	    for (i = 0; i < commsize; i++)
		ranks1[i] = commsize - 1 - i;
	    ierr = MPI_Group_incl(group, commsize, ranks1, &group2);
	    if (ierr != MPI_SUCCESS)
	    {
	        sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_incl #1", ierr);
	        MPITEST_message(MPITEST_NONFATAL, info_buf);
	        MPI_Error_string(ierr, error_string, &size);
	        MPITEST_message(MPITEST_FATAL, error_string);
	        fail++;
	    }       /* Error Test  */
	    loop_cnt++;


	    /* Check the size of the new group */
	    ierr = MPI_Group_size(group2, &result);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_size (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;

	    if (result != commsize)
	    {
		sprintf(info_buf, "MPI_Group_size returned %d, expected %d (comm_index %d)", result, commsize, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;


	    /* Check my rank in the group */
	    ierr = MPI_Group_rank(group2, &result);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_rank (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;
	    if (result != commsize - 1 - MPITEST_current_rank)
	    {
		sprintf(info_buf, "MPI_Group_rank returned %d, expected %d (comm_index %d)", result, MPITEST_current_rank, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;





	    /* Now translate all the new ranks */
	    for (i = 0; i < commsize; i++)
		ranks1[i] = i;
	    ierr = MPI_Group_translate_ranks(group, commsize, ranks1, group2,
			ranks2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_translate_ranks #1 (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;

	    /* Check the translation */
	    verify = 0;
	    for (i = 0; i < commsize; i++)
	    {
		if (ranks2[i] != commsize - 1 - i)
		    verify++;
	    }
	    if (verify != 0)
	    {
		sprintf(info_buf, "%d errors detected in translated ranks #1 (comm_index %d)", verify, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }




	    /* Translate in reverse */
	    ierr = MPI_Group_translate_ranks(group2, commsize, ranks1, group,
			ranks2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_translate_ranks #2 (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;

	    /* Check the translation */
	    verify = 0;
	    for (i = 0; i < commsize; i++)
	    {
		if (ranks2[i] != commsize - 1 - i)
		    verify++;
	    }
	    if (verify != 0)
	    {
		sprintf(info_buf, "%d errors detected in translated ranks #2 (comm_index %d)", verify, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }





	    /* Just translate my own rank */
	    ranks1[0] = MPITEST_current_rank;
	    ierr = MPI_Group_translate_ranks(group2, 1, ranks1, group, ranks2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_translate_ranks #3 (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;

	    /* Check the translation */
	    if (ranks2[0] != commsize - 1 - MPITEST_current_rank)
	    {
		sprintf(info_buf, "1 error detected in translated ranks #3 %d, expected %d (comm_index %d)",
			MPITEST_current_rank,
			commsize - 1 - MPITEST_current_rank,
			comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }






	    /* Translate against MPI_COMM_SELF */
	    ierr = MPI_Comm_group(MPI_COMM_SELF, &groupself);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_group(self) (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;

	    ranks1[0] = MPITEST_current_rank;
	    ierr = MPI_Group_translate_ranks(group, 1, ranks1, groupself, ranks2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_translate_ranks #4 (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;

	    /* Check the translation */
	    if (ranks2[0] != 0)
	    {
		sprintf(info_buf, "1 error detected in translated ranks #4 %d, expected %d got %d (comm_index %d type %d)",
			MPITEST_current_rank,
			commsize - 1 - MPITEST_current_rank,
			ranks2[0],
			comm_index, comm_type);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }




	    /* Reverse translation */
	    ranks1[0] = 0;
	    ierr = MPI_Group_translate_ranks(groupself, 1, ranks1, group2, ranks2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_translate_ranks #5 (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;

	    /* Check the translation */
	    if (ranks2[0] != commsize - 1 - MPITEST_current_rank)
	    {
		sprintf(info_buf, "1 error detected in translated ranks #5 %d, expected %d got %d (comm_index %d type %d)",
			MPITEST_current_rank,
			commsize - 1 - MPITEST_current_rank,
			ranks2[0],
			comm_index, comm_type);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }





	    /* Try against MPI_GROUP_EMPTY  - all should be undefined */
	    for (i = 0; i < commsize; i++)
		ranks1[i] = i;
	    ierr = MPI_Group_translate_ranks(group, commsize, ranks1, 
			MPI_GROUP_EMPTY, ranks2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_translate_ranks #6 (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;

	    /* Check the translation */
	    verify = 0;
	    for (i = 0; i < commsize; i++)
	    {
		if (ranks2[i] != MPI_UNDEFINED)
		    verify++;
	    }
	    if (verify != 0)
	    {
		sprintf(info_buf, "%d errors detected in translated ranks #6 (comm_index %d)", verify, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }




	    /* all against MPI_COMM_SELF - all UNDEFINED but my rank */
	    for (i = 0; i < commsize; i++)
		ranks1[i] = i;
	    ierr = MPI_Group_translate_ranks(group, commsize, ranks1, groupself, ranks2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_translate_ranks #7 (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;

	    /* Check the translation */
	    verify = 0;
	    for (i = 0; i < commsize; i++)
	    {
		if ((i != MPITEST_current_rank) && (ranks2[i] != MPI_UNDEFINED)) {
		    sprintf( info_buf, "rank %d is %d but should be undefined in comparison with self", i, ranks2[i] );
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    verify++;
		}
		else if ((i == MPITEST_current_rank) && (ranks2[i] != 0)) {
		    sprintf( info_buf, "rank %d is %d but should be 0 in comparison with self", i, ranks2[i] );
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    verify++;
		}
	    }
	    if (verify != 0)
	    {
		sprintf(info_buf, "%d errors detected in translated ranks #7 (comm_index %d)", verify, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }





	    /* Finally, the group against itself */
	    for (i = 0; i < commsize; i++)
		ranks1[i] = commsize - i - 1;
	    ierr = MPI_Group_translate_ranks(group, commsize, ranks1, group, ranks2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Group_translate_ranks #7 (comm_index %d type %d)", ierr, comm_index,comm_type);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;

	    /* Check the translation */
	    verify = 0;
	    for (i = 0; i < commsize; i++)
	    {
		if (ranks2[i] != ranks1[i])
		    verify++;
	    }
	    if (verify != 0)
	    {
		sprintf(info_buf, "%d errors detected in translated ranks #8 (comm_index %d)", verify, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }






	    /* Free the groups */
	    ierr = MPI_Group_free(&groupself);
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

	    ierr = MPI_Group_free(&group);
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
	    free(ranks2);
	} /* Node is in this communicator */

    } /* Communicator loop */


    /* report overall results  */

    MPITEST_report(loop_cnt - fail, fail, 0, testname);

    MPI_Finalize();

    return fail;

}/* main() */
