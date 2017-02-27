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
*                          Error test for MPI_Bsend
*
*  Buffered Blocking Sends error tests
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
*10)  Send a message larger than the Bsend attached buffer[MPI_ERR_BUFFER]
*11)  Send a message w/ no buffer attached ...............[MPI_ERR_BUFFER]
*
* In all cases, expect to receive appropriate error.
*
* Test history:
*    1  03/20/96     jh   Created
*
******************************************************************************/
#include "mpitest_cfg.h"
#include "mpitest.h"

int main(int argc, char *argv[])

{
    int
        buffsize,	/* Size, in bytes, of Bsend buffer                   */
        berr,	        /* Error return from MPI buffer calls                */
        bsize,	        /* Returned size of Bsend buffer on detach           */
        dest,	        /* Destination of Bsend message                      */
        pass,           /* counts total number of passes                     */
        fail,	        /* counts total number # of failures                 */
        verify,	        /* counts total number of verify failures            */
        ierr,	        /* return value from MPI calls                       */
        errorclass,	/* error class of ierr                               */
        size;           /* length of error message                           */

    int
        input[10];	/* input to Bsend                                    */

    char
        testname[128],	/* the name of this test                             */
        info_buf[256];	/* for fprintf                                       */
    char error_string[MPI_MAX_ERROR_STRING];

    char *buff;     	/* Character buffer pointer returned by malloc       */
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

    sprintf(testname, "MPI_Bsend_err3:  Use  MPI_COMM_NULL communicator");
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

    /*
     * *  Attempt to Use MPI_COMM_NULL as communicator:
     */
    sprintf(info_buf, "about to call MPI_Bsend w/ MPI_COMM_NULL");
    MPITEST_message(MPITEST_INFO1, info_buf);


    /*--------------------- Allocate buffer for BSEND -----------------------*/
    buffsize = 1024;
    buff = malloc(buffsize);
    if (!buff)
    {
	sprintf(info_buf, "Bsend malloc request failed");
	MPITEST_message(MPITEST_FATAL, info_buf);
    }
    berr = MPI_Buffer_attach(buff, buffsize);
    if (berr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Buffer_attach", berr);
	MPITEST_message(MPITEST_FATAL, info_buf);
    }

    ierr = 0;

    /* Let all nodes do the invalid Bsend  */

    dest = MPITEST_nump - 1;
    /*--------------------------------  Bsend  ------------------------------*/

    ierr = MPI_Bsend(input, 1, MPI_CHAR, dest, 0, MPI_COMM_NULL);

    if (ierr == MPI_SUCCESS)
    {
	fail++;
	sprintf(info_buf, "MPI_Bsend /w MPI_COMM_NULL communicator did not FAIL");
	MPITEST_message(MPITEST_NONFATAL, info_buf);
    }
    else
    {
	MPI_Error_class(ierr, &errorclass);
	if (errorclass != MPI_ERR_COMM)
	{
	    fail++;
	    sprintf(info_buf, "MPI_Bsend /w MPI_COMM_NULL communicator returned %d, expected MPI_ERR_COMM",
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


    /* Detach the buffer, and free the dynamic memory */

    berr = MPI_Buffer_detach(&buff, &bsize);

    if (berr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Buffer_detach", \
		berr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(berr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }	/* Error Test  */


    free(buff);

    /* report overall results  */
    MPITEST_report(pass, fail, verify, testname);

    MPI_Finalize();
    return fail;
}/* main() */
