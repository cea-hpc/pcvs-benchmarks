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

This test creates MPI Keyvals (create and free), and assigns attributes (get,
put, delete) to them on various communicator types as provided bt the MPITEST
environment.

It tests: 1) User copy function, flag=1 / user delete function
          2) MPI_NULL_COPY_FN           / user delete function
          3) MPI_DUP_FN                 / user delete function
          4) User copy function, flag=0 / MPI_NULL_DELETE_FN
on the Keyval_create call, calling Attr_get, Attr_put and Attr_free on
each communicator and a dup'ed communicator and checking for the appropriate
results of each.  The extra_state is incremented in each user-defined
callback function, to validate that it was indeed called.


Test history:
   1  08/28/96     gt       Original version

******************************************************************************/

#include "mpitest_cfg.h"
#include "mpitest.h"


int copy_function1(MPI_Comm oldcomm,
                   int      keyval,
                   void    *extra_state,
                   void    *attribute_val_in,
                   void    *attribute_val_out,
                   int     *flag)
{
    /* According to an MPICH note, if (sizeof(int) < sizeof(void *), then just
       setting the int part of attribute_val_out may leave some dirty bits,
       so in C we need to typecast this just in case.  */
    *(MPI_Aint *)attribute_val_out = (MPI_Aint)attribute_val_in;
    *flag = 1;
    *(MPI_Aint *)extra_state = *(MPI_Aint *)extra_state + 1;
    return MPI_SUCCESS;
}


int copy_function2(MPI_Comm oldcomm,
                   int      keyval,
                   void    *extra_state,
                   void    *attribute_val_in,
                   void    *attribute_val_out,
                   int     *flag)
{
    *flag = 0;
    *(MPI_Aint *)extra_state = *(MPI_Aint *)extra_state + 1;
    return MPI_SUCCESS;
}



int delete_function(MPI_Comm comm,
                    int      keyval,
                    void    *attribute_val,
                    void    *extra_state)
{
    *(MPI_Aint *)extra_state = *(MPI_Aint *)extra_state + 1;
    return MPI_SUCCESS;
}


int main(int argc, char *argv[])
{
    int
	 fail,		/* Counts number of test failures  */
	 loop_cnt,	/* Counts number of tests executed */
	 verify,        /* Counts number of tests to verify*/
	 ierr,		/* Return value from MPI calls     */
	 key1,          /* Keyval returned                 */
	 value1,        /* Value of keyval                 */
	 key2,          /* Keyval returned                 */
	 value2,        /* Value of keyval                 */
	 key3,          /* Keyval returned                 */
	 value3,        /* Value of keyval                 */
	 key4,          /* Keyval returned                 */
	 value4,        /* Value of keyval                 */
	*attr,		/* Attribute returned		   */
	 found,		/* attribute exists		   */
	 test_nump,     /* number of ranks in current comm */
	 comm_index,    /* array index of current comm     */
	 comm_type,     /* index of current comm type      */
	 comm_count,    /* number of communicators to test */
	 type_count,    /* loop counter for data type loop */
	 size;

    MPI_Aint extra1,    /* Will be used to count number of */
	     extra2,    /* times callback functions are    */
	     extra3,    /* called.  Initially 0, correct   */
	     extra4;    /* counts should be 3, 1, 2, 1     */

    MPI_Comm comm,      /* Communicator under test         */
	     comm2;     /* For dup'ing communicator        */

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


    sprintf(testname, "MPI_Keyval1");

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

	    extra1 = 0;
	    extra2 = 0;
	    extra3 = 0;
	    extra4 = 0;

	    /* Keyval 1 ----------------------------------------- */

	    /* Create a Keyval */
	    ierr = MPI_Keyval_create(copy_function1, delete_function, &key1, &extra1);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Keyval_create(1) (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;


	    /* Keyval exists, but no attribute set yet, so expect found=0 */
	    ierr = MPI_Attr_get(comm, key1, &attr, &found);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_get(key1) before put (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    else if (found != 0)
	    {
		fail++;
		sprintf(info_buf, "MPI_Attr_get reported key1 found before initialized (comm_index %d)", comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	    }
	    loop_cnt++;


	    /* Now set a value for the attribute */
	    value1 = MPITEST_me;
	    ierr = MPI_Attr_put(comm, key1, &value1);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_put(key1) (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;


	    /* Now get the value for the attribute */
	    ierr = MPI_Attr_get(comm, key1, &attr, &found);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_get(key1) after put (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    else if (found != 1)
	    {
		fail++;
		sprintf(info_buf, "MPI_Attr_get reported key1 not found after initialized (comm_index %d)", comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	    }
	    else if (*attr != value1)
	    {
		sprintf(info_buf, "MPI_Attr_get(key1) value = %d, expected %d (comm_index %d)", *attr, value1, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }
	    loop_cnt++;


	    /* Now dup the communicator; attribute should follow */
	    ierr = MPI_Comm_dup(comm, &comm2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_dup(key1) (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */


	    /* Now get the value for the attribute */
	    ierr = MPI_Attr_get(comm2, key1, &attr, &found);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_get(key1) on dup'ed comm (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    else if (found != 1)
	    {
		fail++;
		sprintf(info_buf, "MPI_Attr_get reported key1 not found on dup'ed comm (comm_index %d)", comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	    }
	    else if (*attr != value1)
	    {
		sprintf(info_buf, "MPI_Attr_get(key1) on dup'ed comm value = %d, expected %d (comm_index %d)", *attr, value1, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }
	    loop_cnt++;


	    /* Now delete the attribute on comm */
	    ierr = MPI_Attr_delete(comm, key1);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_delete(1) (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;


	    /* Keyval exists, but no longer an attribute set, so expect
	       found=0 */
	    ierr = MPI_Attr_get(comm, key1, &attr, &found);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_get(key1) after deleted (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    else if (found != 0)
	    {
		fail++;
		sprintf(info_buf, "MPI_Attr_get reported key1 found after deleted (comm_index %d)", comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	    }
	    loop_cnt++;


	    /* Now get the value for the attribute on dup'ed comm, should still
	       be there */
	    ierr = MPI_Attr_get(comm2, key1, &attr, &found);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_get(key1) #2 on dup'ed comm (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    else if (found != 1)
	    {
		fail++;
		sprintf(info_buf, "MPI_Attr_get #2 reported key1 not found on dup'ed comm (comm_index %d)", comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	    }
	    else if (*attr != value1)
	    {
		sprintf(info_buf, "MPI_Attr_get(key1) #2 on dup'ed comm value = %d, expected %d (comm_index %d)", *attr, value1, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }
	    loop_cnt++;


	    /* Now free the dup'ed the communicator */
	    ierr = MPI_Comm_free(&comm2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_free(key1) (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */




	    /* Keyval 2 ----------------------------------------- */
	    ierr = MPI_Keyval_create(MPI_NULL_COPY_FN, delete_function, &key2, &extra2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Keyval_create(2) (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;


	    /* Keyval exists, but no attribute set yet, so expect found=0 */
	    ierr = MPI_Attr_get(comm, key2, &attr, &found);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_get(key2) before put (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    else if (found != 0)
	    {
		fail++;
		sprintf(info_buf, "MPI_Attr_get reported key2 found before initialized (comm_index %d)", comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	    }
	    loop_cnt++;


	    /* Now set a value for the attribute */
	    value1 = MPITEST_me;
	    ierr = MPI_Attr_put(comm, key2, &value1);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_put(key2) (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;


	    /* Now get the value for the attribute */
	    ierr = MPI_Attr_get(comm, key2, &attr, &found);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_get(key2) after put (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    else if (found != 1)
	    {
		fail++;
		sprintf(info_buf, "MPI_Attr_get reported key2 not found after initialized (comm_index %d)", comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	    }
	    else if (*attr != value1)
	    {
		sprintf(info_buf, "MPI_Attr_get(key2) value = %d, expected %d (comm_index %d)", *attr, value1, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }
	    loop_cnt++;


	    /* Now dup the communicator; attribute should NOT follow */
	    ierr = MPI_Comm_dup(comm, &comm2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_dup(key2) (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */


	    /* Now get the value for the attribute - should NOT be found */
	    ierr = MPI_Attr_get(comm2, key2, &attr, &found);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_get(key2) on dup'ed comm (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    else if (found != 0)
	    {
		fail++;
		sprintf(info_buf, "MPI_Attr_get reported key2 found on dup'ed comm (comm_index %d)", comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	    }
	    loop_cnt++;


	    /* Now delete the attribute on comm */
	    ierr = MPI_Attr_delete(comm, key2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_delete(1) (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;


	    /* Keyval exists, but no longer an attribute set, so expect
	       found=0 */
	    ierr = MPI_Attr_get(comm, key2, &attr, &found);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_get(key2) after deleted (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    else if (found != 0)
	    {
		fail++;
		sprintf(info_buf, "MPI_Attr_get reported key2 found after deleted (comm_index %d)", comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	    }
	    loop_cnt++;


	    /* Now get the value for the attribute on dup'ed comm, still should
	       not be there */
	    ierr = MPI_Attr_get(comm2, key2, &attr, &found);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_get(key2) #2 on dup'ed comm (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    else if (found != 0)
	    {
		fail++;
		sprintf(info_buf, "MPI_Attr_get #2 reported key2 found on dup'ed comm (comm_index %d)", comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	    }
	    loop_cnt++;


	    /* Now free the dup'ed the communicator */
	    ierr = MPI_Comm_free(&comm2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_free(key1) (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */



	    /* Keyval 3 ----------------------------------------- */
	    ierr = MPI_Keyval_create(MPI_DUP_FN, delete_function, &key3, &extra3);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Keyval_create(3) (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;


	    /* Keyval exists, but no attribute set yet, so expect found=0 */
	    ierr = MPI_Attr_get(comm, key3, &attr, &found);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_get(key3) before put (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    else if (found != 0)
	    {
		fail++;
		sprintf(info_buf, "MPI_Attr_get reported key3 found before initialized (comm_index %d)", comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	    }
	    loop_cnt++;


	    /* Now set a value for the attribute */
	    value1 = MPITEST_me;
	    ierr = MPI_Attr_put(comm, key3, &value1);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_put(key3) (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;


	    /* Now get the value for the attribute */
	    ierr = MPI_Attr_get(comm, key3, &attr, &found);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_get(key3) after put (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    else if (found != 1)
	    {
		fail++;
		sprintf(info_buf, "MPI_Attr_get reported key3 not found after initialized (comm_index %d)", comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	    }
	    else if (*attr != value1)
	    {
		sprintf(info_buf, "MPI_Attr_get(key3) value = %d, expected %d (comm_index %d)", *attr, value1, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }
	    loop_cnt++;


	    /* Now dup the communicator; attribute should follow */
	    ierr = MPI_Comm_dup(comm, &comm2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_dup(key3) (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */


	    /* Now get the value for the attribute */
	    ierr = MPI_Attr_get(comm2, key3, &attr, &found);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_get(key3) on dup'ed comm (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    else if (found != 1)
	    {
		fail++;
		sprintf(info_buf, "MPI_Attr_get reported key3 not found on dup'ed comm (comm_index %d)", comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	    }
	    else if (*attr != value1)
	    {
		sprintf(info_buf, "MPI_Attr_get(key3) on dup'ed comm value = %d, expected %d (comm_index %d)", *attr, value1, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }
	    loop_cnt++;


	    /* Now delete the attribute on comm */
	    ierr = MPI_Attr_delete(comm, key3);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_delete(1) (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;


	    /* Keyval exists, but no longer an attribute set, so expect
	       found=0 */
	    ierr = MPI_Attr_get(comm, key3, &attr, &found);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_get(key3) after deleted (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    else if (found != 0)
	    {
		fail++;
		sprintf(info_buf, "MPI_Attr_get reported key3 found after deleted (comm_index %d)", comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	    }
	    loop_cnt++;


	    /* Now get the value for the attribute on dup'ed comm, should still
	       be there */
	    ierr = MPI_Attr_get(comm2, key3, &attr, &found);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_get(key3) #2 on dup'ed comm (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    else if (found != 1)
	    {
		fail++;
		sprintf(info_buf, "MPI_Attr_get #2 reported key3 not found on dup'ed comm (comm_index %d)", comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	    }
	    else if (*attr != value1)
	    {
		sprintf(info_buf, "MPI_Attr_get(key3) #2 on dup'ed comm value = %d, expected %d (comm_index %d)", *attr, value1, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }
	    loop_cnt++;


	    /* Now free the dup'ed the communicator */
	    ierr = MPI_Comm_free(&comm2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_free(key1) (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */




	    /* Keyval 4 ----------------------------------------- */
	    ierr = MPI_Keyval_create(copy_function2, MPI_NULL_DELETE_FN, &key4, &extra4);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Keyval_create(4) (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;


	    /* Keyval exists, but no attribute set yet, so expect found=0 */
	    ierr = MPI_Attr_get(comm, key4, &attr, &found);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_get(key4) before put (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    else if (found != 0)
	    {
		fail++;
		sprintf(info_buf, "MPI_Attr_get reported key4 found before initialized (comm_index %d)", comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	    }
	    loop_cnt++;


	    /* Now set a value for the attribute */
	    value1 = MPITEST_me;
	    ierr = MPI_Attr_put(comm, key4, &value1);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_put(key4) (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;


	    /* Now get the value for the attribute */
	    ierr = MPI_Attr_get(comm, key4, &attr, &found);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_get(key4) after put (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    else if (found != 1)
	    {
		fail++;
		sprintf(info_buf, "MPI_Attr_get reported key4 not found after initialized (comm_index %d)", comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	    }
	    else if (*attr != value1)
	    {
		sprintf(info_buf, "MPI_Attr_get(key4) value = %d, expected %d (comm_index %d)", *attr, value1, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }
	    loop_cnt++;


	    /* Now dup the communicator; attribute should NOT follow */
	    ierr = MPI_Comm_dup(comm, &comm2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_dup(key4) (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_FATAL, error_string);
		fail++;
	    }       /* Error Test  */


	    /* Now get the value for the attribute, should NOT be found */
	    ierr = MPI_Attr_get(comm2, key4, &attr, &found);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_get(key4) on dup'ed comm (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    else if (found != 0)
	    {
		fail++;
		sprintf(info_buf, "MPI_Attr_get reported key4 found on dup'ed comm (comm_index %d)", comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	    }
	    loop_cnt++;


	    /* Now delete the attribute on comm */
	    ierr = MPI_Attr_delete(comm, key4);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_delete(1) (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    loop_cnt++;


	    /* Keyval exists, but no longer an attribute set, so expect
	       found=0 */
	    ierr = MPI_Attr_get(comm, key4, &attr, &found);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_get(key4) after deleted (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    else if (found != 0)
	    {
		fail++;
		sprintf(info_buf, "MPI_Attr_get reported key4 found after deleted (comm_index %d)", comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	    }
	    loop_cnt++;


	    /* Now get the value for the attribute on dup'ed comm, should NOT
	       be there */
	    ierr = MPI_Attr_get(comm2, key4, &attr, &found);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Attr_get(key4) #2 on dup'ed comm (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    else if (found != 0)
	    {
		fail++;
		sprintf(info_buf, "MPI_Attr_get #2 reported key4 found on dup'ed comm (comm_index %d)", comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	    }
	    loop_cnt++;


	    /* Now free the dup'ed the communicator */
	    ierr = MPI_Comm_free(&comm2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Comm_free(key1) (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */



	    /* Free all the Keyvals ---------------------------------------- */
	    ierr = MPI_Keyval_free(&key1);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Keyval_free(1) (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    if (key1 != MPI_KEYVAL_INVALID)
	    {
		sprintf(info_buf, "key1 not set to MPI_KEYVAL_INVALID by MPI_Keyval_free (comm_index %d)", comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;



	    ierr = MPI_Keyval_free(&key2);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Keyval_free(2) (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    if (key2 != MPI_KEYVAL_INVALID)
	    {
		sprintf(info_buf, "key2 not set to MPI_KEYVAL_INVALID by MPI_Keyval_free (comm_index %d)", comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;


	    ierr = MPI_Keyval_free(&key3);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Keyval_free(3) (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    if (key3 != MPI_KEYVAL_INVALID)
	    {
		sprintf(info_buf, "key3 not set to MPI_KEYVAL_INVALID by MPI_Keyval_free (comm_index %d)", comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;

	    ierr = MPI_Keyval_free(&key4);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Keyval_free(4) (comm_index %d)", ierr, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }       /* Error Test  */
	    if (key4 != MPI_KEYVAL_INVALID)
	    {
		sprintf(info_buf, "key4 not set to MPI_KEYVAL_INVALID by MPI_Keyval_free (comm_index %d)", comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    loop_cnt++;


	    /* Check the number of times the callback
	       functions were called; this is different for each */
	    if (extra1 != 3)
	    {
		sprintf(info_buf, "callback functions called %d times for key1,  expected 3 (comm_index %d)", extra1, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    if (extra2 != 1)
	    {
		sprintf(info_buf, "callback functions called %d times for key2, expected 1 (comm_index %d)", extra2, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    if (extra3 != 2)
	    {
		sprintf(info_buf, "callback functions called %d times for key3, expected 2 (comm_index %d)", extra3, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }
	    if (extra4 != 1)
	    {
		sprintf(info_buf, "callback functions called %d times for key4, expected 1 (comm_index %d)", extra4, comm_index);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }

	    MPITEST_free_communicator(comm_type, &comm);

	} /* Node is in this communicator */

    } /* Communicator loop */


    /* report overall results  */

    MPITEST_report(loop_cnt - fail, fail, verify, testname);

    MPI_Finalize();

    return fail;

}/* main() */
