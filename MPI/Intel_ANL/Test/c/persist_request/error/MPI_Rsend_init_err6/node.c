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
*                          Error test for MPI_Rsend_init
*
*  MPI_Rsend_init error tests
*  --------------------------------------------------------
* 1)  Destination non-existent rank (rank negative) ..... [MPI_ERR_RANK]
* 2)  Destination rank greater than group size .......... [MPI_ERR_RANK]
* 3)  Call with MPI_COMM_NULL ........................... [MPI_ERR_COMM]
* 4)  Call with a freed communicator .................... [MPI_ERR_COMM]
* 5)  Use a tag < 0 ..................................... [MPI_ERR_TAG ]
* 6)  Use a tag larger than Attr<MPI_TAG_UB> ............ [MPI_ERR_TAG ]
* 7)  Use a negative count argument ..................... [MPI_ERR_COUNT]
* 8)  Use a non-existent data type  <MPI_DATATYPE_NULL>.. [MPI_ERR_TYPE]
* 9)  Use mismatching type  at sender and receiver ...... [MPI_ERR_TYPE]
*-------------------------------------------------------------------------
*
* In all cases, expect to receive appropriate error.
*
* Test history:
*    1  04/12/96     jh   Created
*
*  NOTES:
*          Reference:  MPI_Standard, Section 3.2.3
*                      Valid tag range is 0,...MPI_Attr_get of <MPI_TAG_UB>
*
******************************************************************************/


#include "mpitest_cfg.h"
#include "mpitest.h"

int main(int argc, char *argv[])

{
    int
        dest,   	/* Destination of Rsend_init message                 */
        pass,   	/* counts total number of passes                     */
        fail,	        /* counts total number # of failures                 */
        verify,  	/* counts total number of verify failures            */
        ierr,	        /* return value from MPI calls                       */
        errorclass,	/* error class of ierr                               */
        size,	        /* length of error string message                    */
        messflag,	/* error flag for get MPI_TAG_UB attribute call      */
        *maxtag,   	/* maximum tag value                                 */
        messtag;	/* tag value used in Rsend_init                          */

    int
        input[10];	/* input to Rsend_init                                   */

    char
        testname[128],	/* the name of this test                             */
        info_buf[256];	/* for sprintf                                       */
    char error_string[MPI_MAX_ERROR_STRING];

    MPI_Request
        request;        /*  MPI request structure                            */

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

    sprintf(testname, "MPI_Rsend_init_err6:  Call with tag greater than max allowed");
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
     * MPI_TAG_UB  is not the upper bound, it is an attribute.  You use it as
     * an input parameter to the MPI_Attr_get function to get the actual upper
     * bound, which in this case, is returned in maxtag
     */
    messtag = -1;

    MPI_Attr_get(MPI_COMM_WORLD, MPI_TAG_UB, &maxtag, &messflag);

    if (!messflag)	/* If messflag false, did not get MPI_TAG_UB value */
    {
	fail++;
	sprintf(info_buf, "MPI_Attr_get failed ");
	MPITEST_message(MPITEST_FATAL, info_buf);
    }

    messtag = 1 + *maxtag;


    /*
     * *  Attempt to  Call with tag greater than Attr_get<MPI_TAG_UB>:
     */
    sprintf(info_buf, "About to call MPI_Rsend_init w/  tag greater than MPI_TAG_UB attribute ( > %d)  Tag =  %d  ", *maxtag, messtag);
    MPITEST_message(MPITEST_INFO1, info_buf);


    ierr = 0;

    /* Let all nodes do the invalid Rsend_init  */

    dest = MPITEST_nump - 1;

    /*----------------------------------  Rsend_init  ---------------------------*/

    ierr = MPI_Rsend_init(input, 1, MPI_CHAR, dest, messtag, MPI_COMM_WORLD, &request);

    if (ierr == MPI_SUCCESS)
    {
	fail++;
	sprintf(info_buf, "MPI_Rsend_init /w   Call with tag greater than MPI_TAG_UB attribute (%d/%d) did not FAIL", *maxtag, messtag);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
    }
    else
    {
	MPI_Error_class(ierr, &errorclass);
	if (errorclass != MPI_ERR_TAG)
	{
	    fail++;
	    sprintf(info_buf, "MPI_Rsend_init /w  Call with tag greater than MPI_TAG_UB attribute returned %d, expected  MPI_ERR_TAG",
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

    /* report overall results  */
    MPITEST_report(pass, fail, verify, testname);

    MPI_Finalize();
    return fail;
}/* main() */
