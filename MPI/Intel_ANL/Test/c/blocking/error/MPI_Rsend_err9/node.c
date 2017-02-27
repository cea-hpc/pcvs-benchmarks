/*-----------------------------------------------------------------------------
MESSAGE PASSING INTERFACE TEST CASE SUITE

Copyright - 1996 Intel Corporation

Intel Corporation hereby gra    jerr,
nts a non-exclusive license under Intel's
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
*                          Error test for MPI_Rsend
*
*  Blocking Rsends error tests
*  -----------------------------------
* 1)  Destination non-existent rank (rank negative) ..... [MPI_ERR_RANK]
* 2)  Destination rank greater than group size .......... [MPI_ERR_RANK]
* 3)  Call with MPI_COMM_NULL ........................... [MPI_ERR_COMM]
* 4)  Call with a freed communicator .................... [MPI_ERR_COMM]
* 5)  Use a tag < 0 ..................................... [MPI_ERR_TAG ]
* 6)  Use a tag larger than Attr<MPI_TAG_UB> ............ [MPI_ERR_TAG ]
* 7)  Use a negative count argument ..................... [MPI_ERR_COUNT]
* 8)  Use a non-existent data type ...................... [MPI_ERR_TYPE]
* 9)  Use mismatching type  at sender and receiver ...... [MPI_ERR_TYPE]
*-------------------------------------------------------------------------
*
* Test history:
*   created  03/96  jh
*
* NOTES:
*
*  03/20/96  Data mismatch between Rsend and receive NOT being detected
******************************************************************************/

#include "mpitest_cfg.h"
#include "mpitest.h"

int main(int argc, char *argv[])
{
    int
        dest,	        /* Destination of Rsend message                      */
        i,      	/* general for loop index                            */
        pass,    	/* counts total number of passes                     */
        fail,    	/* counts total number # of failures                 */
        verify,   	/* counts total number of verify failures            */
        ierr,   	/* return value from MPI calls                       */
        errorclass,	/* error class of ierr                               */
        size;	        /* length of error string                            */

    char
        testname[128],	/* the name of this test                             */
        info_buf[256];	/* for sprintf                                       */
    char error_string[MPI_MAX_ERROR_STRING];
   
    MPI_Status
	recv_stat;	/* Receive Source/Tag  information                   */

    int
        int_recv[100];	/* Array of values to receive                        */

    float
        float_send[100];	/* Array of values to Rsend                   */
  
    /*-----------------------------------------------------------------------*/
    /* Initialize arrays */

    for (i = 0; i < 10; i++)
    {
        float_send[i] = 12345.6789;
	int_recv[i] = -1;
    }

    /*-----------------------------------------------------------------------*/

    /*
     * *  Initialize the MPI environment and test environment.
     */

    ierr = MPI_Init(&argc, &argv);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr);
	MPITEST_message(MPITEST_FATAL, info_buf);
    }

    sprintf(testname, "MPI_Rsend_err9: Mismatched data types between send and receive  ");
    MPITEST_init(argc, argv);
    if (MPITEST_me == 0)
    {
	sprintf(info_buf, "Starting %s test", testname);
	MPITEST_message(MPITEST_INFO0, info_buf);
    }

    pass = 0;
    fail = 0;
    verify = 0;

    /*
     * *  Set an errorhandler so we get control back.
     */
    ierr = MPI_Errhandler_set(MPI_COMM_WORLD, MPI_ERRORS_RETURN);
    if (ierr != MPI_SUCCESS)
    {
	fail++;
	sprintf(info_buf, "MPI_Errorhandler_set returned %d", ierr);
	MPITEST_message(MPITEST_FATAL, info_buf);
    }

    /* We only need 2 nodes to test the mismatch  */
    if (MPITEST_nump < 2)
    {
	sprintf(info_buf, "Not enough ranks to test Rsend/Recv with mismatched data types");
	MPITEST_message(MPITEST_FATAL, info_buf);
    }

    sprintf(info_buf, "about to call MPI_Rsend w/  Rsend/Recv with mismatched data types between send and receive");

    MPITEST_message(MPITEST_INFO1, info_buf);


    /* For this test we need two nodes, so we can post a receive */

    if (MPITEST_current_rank < 2)
    {

	/*------------------- Initialize flags, and buffers -----------------*/

	ierr = 0;

	/*--------------------------------  Rsend  ---------------------------*/

	if (MPITEST_me == 0)
	{

	    dest = 1;

	    ierr = MPI_Rsend(float_send, 10, MPI_FLOAT, dest, 1234, MPI_COMM_WORLD);

	}

	/*---------------------------------  RECEIVE  -----------------------*/

	if (MPITEST_me == 1)
	{

	    ierr = MPI_Recv(int_recv, 10, MPI_INT, 0, MPI_ANY_TAG, \
			    MPI_COMM_WORLD, &recv_stat);

	    if (ierr == MPI_SUCCESS)
	    {
		fail++;
		sprintf(info_buf, "MPI_Rsend/Recv /w Mismatched data type on receive did not FAIL");
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	    }
	    else
	    {
		MPI_Error_class(ierr, &errorclass);
		if (errorclass != MPI_ERR_TYPE)
		{
		    fail++;
		    sprintf(info_buf, "MPI_Rsend/Recv /w Mismatched data type on receive, returned  %d, expected MPI_ERR_TYPE",
			    errorclass);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_NONFATAL, error_string);
		}
		else
		{
		    pass++;
		    sprintf(info_buf, "ierr = %d, errorclass = %d", ierr,
			    errorclass);
		    MPITEST_message(MPITEST_INFO2, info_buf);
		    MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_INFO1, error_string);
		}
	    }
	}

    }


    /*
     * Only nodes 0 and 1 play in this game but everybody has to call
     * MPITEST_report, so do a dummy call for the other nodes
     */
    if (MPITEST_current_rank > 1)
    {
	MPITEST_report(0, 0, 0, testname);
    }

    /* Do the real call for nodes 0 and 1  */
    if (MPITEST_current_rank < 2)
    {
	/* report overall results  */
	MPITEST_report(pass, fail, verify, testname);
    }

    MPI_Finalize();
    return fail;
}/* main() */
