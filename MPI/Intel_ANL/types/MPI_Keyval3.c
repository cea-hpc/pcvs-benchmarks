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
		  Test for MPI keyvals and attributes.

This test verifies that errors from copy or delete functions are returned to
an application.  It checks that the correct error is returned.

Test history:
   1  08/28/96     gt       Original version

******************************************************************************/

#include "mpitest_cfg.h"
#include "mpitest.h"

int copy_function(MPI_Comm oldcomm,
                  int      keyval,
                  void    *extra_state,
                  void    *attribute_val_in,
                  void    *attribute_val_out,
                  int     *flag)
{
    return MPI_ERR_OTHER;
}

int delete_function(MPI_Comm comm,
                    int      keyval,
                    void    *attribute_val,
                    void    *extra_state)
{
    /* This is here so we can eventually delete the attribute */
    if ( *(MPI_Aint *)extra_state == 0)
    {
	return MPI_ERR_OTHER;
    }
    return MPI_SUCCESS;
}


int main(int argc, char *argv[])
{
    int
	 fail,		/* Counts number of test failures  */
	 loop_cnt,	/* Counts number of tests executed */
	 verify,        /* Counts number of tests to verify*/
	 ierr,		/* Return value from MPI calls     */
	 key,           /* Keyval returned                 */
	 value,         /* Value of keyval                 */
	*attr,		/* Attribute returned		   */
	 found,		/* attribute exists		   */
	 size;

    MPI_Aint extra;
    MPI_Comm comm1,
	     comm2;

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


    sprintf(testname, "MPI_Keyval3");

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
    extra = 0;

    ierr = MPI_Keyval_create(copy_function, delete_function, &key, &extra);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Keyval_create", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */
    loop_cnt++;


    /* Dup COMM_WORLD to use as test communicator */
    ierr = MPI_Comm_dup(MPI_COMM_WORLD, &comm1);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_dup", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_FATAL, error_string);
    }       /* Error Test  */
    loop_cnt++;


    /* Now set a value for the attribute */
    value = 0;
    ierr = MPI_Attr_put(comm1, key, &value);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_put", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */
    loop_cnt++;


    /* Try to dup it; should not be allowed */
    ierr = MPI_Comm_dup(comm1, &comm2);
    if (ierr != MPI_ERR_OTHER)
    {
	sprintf(info_buf, "Unexpected return code (%d) from MPI_Comm_dup - expected MPI_ERR_OTHER", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
    }       /* Error Test  */
    loop_cnt++;


    /* Now try to delete the attribute; should not be allowed */
    ierr = MPI_Attr_delete(comm1, key);
    if (ierr != MPI_ERR_OTHER)
    {
	sprintf(info_buf, "Unexpected return code (%d) from MPI_Attr_delete - expected MPI_ERR_OTHER", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */
    loop_cnt++;


    /* Now set a new value for the attribute; should not be allowed */
    value = 1;
    ierr = MPI_Attr_put(comm1, key, &value);
    if (ierr != MPI_ERR_OTHER)
    {
	sprintf(info_buf, "Unexpected return code (%d) from MPI_Attr_put", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */
    loop_cnt++;



    /* Try to free the comm, should not be allowed */
    ierr = MPI_Comm_free(&comm1);
    /* WDG-11/03: Formally, an error return from the attribute routines
       makes the program erroneous (see clarification in MPI 1.2).  
       Thus, rather than testing
       for an error return of MPI_ERR_OTHER, we must check only for
       MPI_SUCCESS
    */
    if (ierr == MPI_SUCCESS)
    {
	sprintf(info_buf, "Unexpected return code (%d) from MPI_Comm_free - expected MPI_ERR_OTHER", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
    }       /* Error Test  */
    loop_cnt++;


    extra = 2;
    /* Now try to delete the attribute; should be allowed */
    ierr = MPI_Attr_delete(comm1, key);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_delete after free", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */
    loop_cnt++;


    /* Try to free the comm, should be allowed */
    ierr = MPI_Comm_free(&comm1);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_free", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
    }       /* Error Test  */
    loop_cnt++;


    /* Free the keyval */
    ierr = MPI_Keyval_free(&key);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Keyval_free(2)", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_NONFATAL, error_string);
	fail++;
    }       /* Error Test  */
    if (key != MPI_KEYVAL_INVALID)
    {
	sprintf(info_buf, "key not set to MPI_KEYVAL_INVALID by MPI_Keyval_free");
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	fail++;
    }
    loop_cnt++;


    /* report overall results  */

    MPITEST_report(loop_cnt - fail, fail, verify, testname);

    MPI_Finalize();

    return fail;

}/* main() */
