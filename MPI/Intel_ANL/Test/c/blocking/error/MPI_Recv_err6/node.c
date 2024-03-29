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
*                          Error test for MPI_Recv
*
* This test verifies that the correct error is returned if MPI_Recv is
* called with an invalid argument.
**
*
*  Blocking Receive error tests
*  -----------------------------------
* 1)  Source  non-existent rank (rank negative) ......... [MPI_ERR_RANK]
* 2)  Source rank greater than group size ............... [MPI_ERR_RANK]
* 3)  Call with MPI_COMM_NULL ........................... [MPI_ERR_COMM]
* 4)  Call with a freed communicator .................... [MPI_ERR_COMM]
* 5)  Use a tag < 0 ..................................... [MPI_ERR_TAG ]
* 6)  Use a tag larger than MPI_TAG_UB .................. [MPI_ERR_TAG ]
* 7)  Use a negative count argument ..................... [MPI_ERR_COUNT]
* 8)  Use a non-existent data type ...................... [MPI_ERR_TYPE]
* 9)  Try to receive a message larger than Recv count ... [MPI_ERR_TRUNCATE]
*
*
* In all cases, expect to receive appropriate error.
*
* Test history:
*    1  04/03/96     jh   Created
*
*
*  NOTES:
*     Reference:  MPI_Standard, Section 3.2.3
*     Valid tag range is 0,...MPI_Attr_get of <MPI_TAG_UB>
*
*     04/22/96  Error  6, Tag too large does not return an error, nodes
*     hang trying to receive.
*
******************************************************************************/
#define  TEST_TYPE   "MPI_Recv"
#define  PROG_NAME   "MPI_Recv_err06"
#define  MESS01      "Call with tag greater than max allowed"
#define  MESS02      "Call with TAG > MPI_TAG_UB attribute"
#define  ERR_NAM     "MPI_ERR_TAG"


#include "mpitest_cfg.h"
#include "mpitest.h"

int main(int argc, char *argv[])

{
    int
        source,	        /* Source of Recv message                            */
        pass,           /* counts total number of passes                     */
        fail,	        /* counts total number # of failures                 */
        verify,    	/* counts total number of verify failures            */
        ierr,	        /* return value from MPI calls                       */
        errorclass,	/* error class of ierr                               */
        size,           /* length of error message                           */
        messflag,	/* error flag for get MPI_TAG_UB attribute call      */
        *maxtag,   	/* maximum tag value                                 */
        messtag;	/* tag value used in Send                            */

    int
        output[10];	/* output from Recv                                  */

    char
        testname[128],	/* the name of this test                             */
        info_buf[256];	/* for sprintf                                       */
    char error_string[MPI_MAX_ERROR_STRING];

    MPI_Status          /* Receive status (source/tag)                       */
        recv_stat;
  
    /*-----------------------------------------------------------------------*/

    /*   
    **  Initialize the MPI environment and test environment.
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

    /*-----------------------  Get MPI_TAG_UB attribute ---------------------*/

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


    /*-------------------------------  Receive ------------------------------*/

    sprintf(info_buf, "About to call %s  w/ %s", TEST_TYPE, MESS02);
    MPITEST_message(MPITEST_INFO1, info_buf);

    ierr = 0;

    /* Let all nodes do the invalid Recv  */

    source = 0;
    messtag = *maxtag + 1;

    ierr = MPI_Recv(output, 1, MPI_CHAR, source, messtag, \
		     MPI_COMM_WORLD, &recv_stat);

    if (ierr == MPI_SUCCESS)
    {
	fail++;
	sprintf(info_buf, "%s /w %s (%d/%d) did not FAIL", TEST_TYPE, MESS02, *maxtag, messtag );
	MPITEST_message(MPITEST_NONFATAL, info_buf);
    }
    else
    {
	MPI_Error_class(ierr, &errorclass);
	if (errorclass != MPI_ERR_TAG)
	{
	    fail++;
	    sprintf(info_buf, "%s /w %s  Returned: %d,  Expected:  %s",TEST_TYPE, MESS02, errorclass, ERR_NAM);
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
