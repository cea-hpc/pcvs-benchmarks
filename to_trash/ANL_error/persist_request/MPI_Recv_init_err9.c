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
*                          Error test for MPI_Recv_init
*
*  Nonblocking Receive error tests
*  -----------------------------------
* 1)  Destination non-existent rank (rank negative) ..... [MPI_ERR_RANK]
* 2)  Destination rank greater than group size .......... [MPI_ERR_RANK]
* 3)  Call with MPI_COMM_NULL ........................... [MPI_ERR_COMM]
* 4)  Call with a freed communicator .................... [MPI_ERR_COMM]
* 5)  Use a tag < 0 ..................................... [MPI_ERR_TAG ]
* 6)  Use a tag larger than Attr<MPI_TAG_UB> ............ [MPI_ERR_TAG ]
* 7)  Use a negative count argument ..................... [MPI_ERR_COUNT]
* 8)  Use a non-existent data type ...................... [MPI_ERR_TYPE]
* 9)  Try to receive a message larger than Recv_init count .. [MPI_ERR_TRUNCATE]
*-------------------------------------------------------------------------
*
* Test history:
*   created  04/96  jh
*
* NOTES:
*        Recv_init test 9, sends messages from node zero to each of the
*        other nodes.  Each message is longer than the preceding one by 10+
*        bytes.  The message from node 0 to node 1 is one byte; the message 
*        to node 2 is 12 bytes, the message from node zero to node 14 would
*        be 144 bytes.
*
******************************************************************************/
#define  TEST_TYPE   "MPI_Recv_init"
#define  PROG_NAME   "MPI_Recv_init_err9"
#define  MESS01      "Try to receive a message larger than Recv_init count"
#define  ERR_NAM     "MPI_ERR_TRUNCATE"

#include "mpitest_cfg.h"
#include "mpitest.h"

int main(int argc, char *argv[])
{
    int
        dest,	        /* Destination of Send message                       */
        i,      	/* general for loop index                            */
        pass,    	/* counts total number of passes                     */
        fail,    	/* counts total number # of failures                 */
        verify,   	/* counts total number of verify failures            */
        ierr, jerr,	/* return value from MPI calls                       */
        errorclass,	/* error class of ierr                               */
        size;	        /* length of error string                            */

    char
        testname[128],	/* the name of this test                             */
        info_buf[256];	/* for sprintf                                       */
    char error_string[MPI_MAX_ERROR_STRING];
   

    MPI_Status
	recv_stat;	/* Receive Source/Tag  information                   */

    int
        buffer[5000];	/* Array of values to send/receive                   */

    char
        errstring[128];	/* Extended error string                             */

    int
        strlen;  	/* Length of message in errstring                    */

    MPI_Request
        recv_request;   /*  MPI request structure                            */

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

    sprintf(testname, "%s:  %s ", PROG_NAME, MESS01);


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

    /* For this test we need two nodes, so we can post a receive */

    if (MPITEST_nump < 2)
    {
	sprintf(info_buf, "Insufficient ranks to test %s", MESS01);
	MPITEST_message(MPITEST_FATAL, info_buf);
    }

    sprintf(info_buf, "About to call %s  w/ %s", TEST_TYPE, MESS01);

    MPITEST_message(MPITEST_INFO1, info_buf);


	ierr = 0;

	/*--------------------------------  Send  ---------------------------*/

	if (MPITEST_me == 0)
	{
            
	  for (dest=1; dest < MPITEST_nump; dest++)
	  {
	    ierr = MPI_Send(buffer, dest+(dest-1)*10, MPI_INT, dest, 0, MPI_COMM_WORLD);
	  pass++;

	  }

	}
        else
	/*---------------------------------  RECEIVE  -----------------------*/

	{
	    ierr = MPI_Recv_init(buffer, (MPITEST_current_rank-1)*7, MPI_INT, 0, 0,			    MPI_COMM_WORLD, &recv_request);

            ierr = MPI_Start(&recv_request);
            ierr = MPI_Wait(&recv_request, &recv_stat);
            if (ierr == MPI_ERR_IN_STATUS)
                ierr = recv_stat.MPI_ERROR;

	    if (ierr == MPI_SUCCESS)
	    {
		fail++;
	sprintf(info_buf, "%s /w %s  did not FAIL", TEST_TYPE, MESS01);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	    }
	    else
	    {
		MPI_Error_class(ierr, &errorclass);
		if (errorclass != MPI_ERR_TRUNCATE)
		{
		    fail++;

	    sprintf(info_buf, "%s /w %s  Returned: %d,  Expected:  %s",TEST_TYPE, MESS01, errorclass, ERR_NAM);
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


/* report overall results  */

    MPITEST_report(pass, fail, verify, testname);

    MPI_Finalize();
    return fail;
}/* main() */
