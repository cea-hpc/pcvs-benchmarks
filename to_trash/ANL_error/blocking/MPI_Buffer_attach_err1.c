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
*           Error test for MPI_BUFFER Attach
*
*  Buffer Attach errors
*  -----------------------------------
* 1)  Attach multiple buffers to one process ............ [MPI_ERR_BUFFER]
* 2)  Attach a buffer of negative length ................ [MPI_ERR_BUFFER]
*-------------------------------------------------------------------------
*
* Test history:
*   created  04/96  jh
*
******************************************************************************/
#define  TEST_TYPE   "MPI_Buffer_attach"
#define  PROG_NAME   "MPI_Buffer_attach_err1"
#define  MESS01      " Attach multiple buffers to one process"
#define  ERR_NAM     "MPI_ERR_BUFFER"

#include "mpitest_cfg.h"
#include "mpitest.h"

int main(int argc, char *argv[])
{
    int
        buffsize,	/* Size, in bytes, of Bsend buffer                   */
        berr,   	/* Error return from MPI buffer calls                */
        bsize,  	/* Returned size of Bsend buffer on detach           */
        pass,    	/* counts total number of passes                     */
        fail,    	/* counts total number # of failures                 */
        verify,   	/* counts total number of verify failures            */
        ierr, 	        /* return value from MPI calls                       */
        errorclass,	/* error class of ierr                               */
         size;	        /* length of error string                            */

    char
        testname[128],	/* the name of this test                             */
        info_buf[256];	/* for sprintf                                       */
    char error_string[MPI_MAX_ERROR_STRING];
   
    char
       *buff,     	/* Buffer pointer for malloc for Bsend buffer        */
       *buff2;     	/* Buffer pointer for malloc for second Bsend buffer */

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

    sprintf(info_buf, "About to call %s  w/ %s", TEST_TYPE, MESS01);



    MPITEST_message(MPITEST_INFO1, info_buf);

    /*--------------------- Allocate buffer for BSEND -----------------------*/

    buffsize = 4096;

    /*
     * Malloc allocates a buffer on a big address boundary, so you can
     * store any kind of variable in it.
     */
    buff = malloc(buffsize);
    if(!buff)
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


    /*----------Allocate and attempt to attach second buffer  ---------------*/


    sprintf(info_buf, "About to call %s  w/ %s", TEST_TYPE, MESS01);

    buff2 = malloc(buffsize);
    if(!buff2)
    {
	sprintf(info_buf, "malloc request failed");
	MPITEST_message(MPITEST_FATAL, info_buf);
    }
    ierr = MPI_Buffer_attach(buff2, buffsize);

    if (ierr == MPI_SUCCESS)
    {
	fail++;
	sprintf(info_buf, "%s /w %s  did not FAIL", TEST_TYPE, MESS01);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
    }
    else
    {
	MPI_Error_class(ierr, &errorclass);
	if (errorclass != MPI_ERR_BUFFER)
	{
	    fail++;

	    sprintf(info_buf, "%s /w %s  Returned: %d (%s),  Expected:  %s",TEST_TYPE, MESS01, errorclass, MPITEST_GetErrName( errorclass ), ERR_NAM);
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
/*-----------------------------  Detach Buffer  -----------------------------*/

    /* Detach the buffer, and free the dynamic memory */

    berr = MPI_Buffer_detach(&buff, &bsize);

    if (berr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Buffer_detach", berr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(berr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }	/* Error Test  */

    free(buff);
    free(buff2);

    /* report overall results  */
    MPITEST_report(pass, fail, verify, testname);

    MPI_Finalize();
    return fail;
}/* main() */
