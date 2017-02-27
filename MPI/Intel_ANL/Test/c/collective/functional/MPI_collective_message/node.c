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
                          Test for MPI_collective_message

This test validates that collective operations do not interfere with
ongoing message traffic.  The ranks are split into even-odd pairs (minus
the last rank if an odd number of ranks).  Before the collective operation,
the even-numbered rank does an Isend to the odd ranks followed by an Irecv
from ANY_RANK/ANY_TAG. After the collective operation, The odd node Recv's the
message, then sends a message to the even rank, while the even rank waits
for the Irecv to complete.

There should be no interference from the multiple calls.  Incorrect data
received or an MPI error constitutes failure.

Test history:
   1  09/03/96     gt       Original version

******************************************************************************/

#include "mpitest_cfg.h"
#include "mpitest.h"

#define MPITEST_SEND_LENGTH 4096

/*-----------------------------------------------------------------------------
Pre-collective operation:

The even rank (partner = 0) Isends a message to the even rank, and posts
an Irecv for ANY_SOURCE/ANY_TAG.
-----------------------------------------------------------------------------*/
int precoll(int		 partner,
	    int		 side,
	    int		*send_buffer,
	    int		*recv_buffer,
	    int		 tag,
	    MPI_Request	*requests,
	    MPI_Request	*requestr)
{
    int
	fail,
	ierr,
	size;

    char
	  info_buf[256];/* buffer for passing mesages to MPITEST  */
    char error_string[MPI_MAX_ERROR_STRING];

    fail = 0;
    send_buffer[0] = tag;
    recv_buffer[0] = 0;
    if (partner != MPI_UNDEFINED)
    {
	if (side == 0)
        {
            ierr = MPI_Isend(send_buffer, MPITEST_SEND_LENGTH, MPI_INT, partner,
			     tag, MPI_COMM_WORLD, requests);
    	    if (ierr != MPI_SUCCESS)
	    {
	        sprintf(info_buf, "Non-zero return code (%d) from MPI_Isend", ierr);
	        MPITEST_message(MPITEST_NONFATAL, info_buf);
	        MPI_Error_string(ierr, error_string, &size);
	        MPITEST_message(MPITEST_FATAL, error_string);
	        fail++;
            }       /* Error Test  */
	    ierr = MPI_Irecv(recv_buffer, MPITEST_SEND_LENGTH, MPI_INT, 
			     MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD,
			     requestr);
    	    if (ierr != MPI_SUCCESS)
	    {
	        sprintf(info_buf, "Non-zero return code (%d) from MPI_Irecv", ierr);
	        MPITEST_message(MPITEST_NONFATAL, info_buf);
	        MPI_Error_string(ierr, error_string, &size);
	        MPITEST_message(MPITEST_FATAL, error_string);
	        fail++;
            }       /* Error Test  */
	}
    }

    return(fail);
}

/*-----------------------------------------------------------------------------
Post-collective operation:

The even rank (partner = 0) waits for the Irecv posted earlier to complete.

The odd rank receives the message sent earlier, then sends a message to the
even rank to saitsfy the Irecv posted earlier.
-----------------------------------------------------------------------------*/
int postcoll(int	  partner,
	     int	  side,
	     int	 *send_buffer,
             int	 *recv_buffer,
             int	  tag,
	     MPI_Request *requests,
	     MPI_Request *requestr)

{
    int
	fail,
	ierr,
	size;

    char
	  info_buf[256];/* buffer for passing mesages to MPITEST  */
    char error_string[MPI_MAX_ERROR_STRING];

    MPI_Status
	stat;

    fail = 0;
    send_buffer[0] = tag;
    if (partner != MPI_UNDEFINED)
    {
	if (side == 1)
	{
	    /* WDG - The following initialize was improperly placed outside 
	       of the size.eq.1 test (found by Rajeev Thakur) */
	    recv_buffer[0] = 0;
            ierr = MPI_Recv(recv_buffer, MPITEST_SEND_LENGTH, MPI_INT, partner,
			    tag, MPI_COMM_WORLD, &stat);
    	    if (ierr != MPI_SUCCESS)
	    {
	        sprintf(info_buf, "Non-zero return code (%d) from MPI_Recv", ierr);
	        MPITEST_message(MPITEST_NONFATAL, info_buf);
	        MPI_Error_string(ierr, error_string, &size);
	        MPITEST_message(MPITEST_FATAL, error_string);
	        fail++;
            }       /* Error Test  */
	    if (recv_buffer[0] != tag)
	    {
	        sprintf(info_buf, "MPI_Recv received %d, expected %d", recv_buffer[0], tag);
	        MPITEST_message(MPITEST_NONFATAL, info_buf);
	        fail++;
	    }
            ierr = MPI_Send(send_buffer, MPITEST_SEND_LENGTH, MPI_INT, partner,
			     tag, MPI_COMM_WORLD);
    	    if (ierr != MPI_SUCCESS)
	    {
	        sprintf(info_buf, "Non-zero return code (%d) from MPI_Send", ierr);
	        MPITEST_message(MPITEST_NONFATAL, info_buf);
	        MPI_Error_string(ierr, error_string, &size);
	        MPITEST_message(MPITEST_FATAL, error_string);
	        fail++;
            }       /* Error Test  */
	}
	else
	{
            ierr = MPI_Wait(requestr, &stat);
    	    if (ierr != MPI_SUCCESS)
	    {
	        sprintf(info_buf, "Non-zero return code (%d) from MPI_Wait(Irecv)", ierr);
	        MPITEST_message(MPITEST_NONFATAL, info_buf);
	        MPI_Error_string(ierr, error_string, &size);
	        MPITEST_message(MPITEST_FATAL, error_string);
	        fail++;
            }       /* Error Test  */
	    if (recv_buffer[0] != tag)
	    {
	        sprintf(info_buf, "MPI_Irecv received %d, expected %d", recv_buffer[0], tag);
	        MPITEST_message(MPITEST_NONFATAL, info_buf);
	        fail++;
	    }
            ierr = MPI_Wait(requests, &stat);
    	    if (ierr != MPI_SUCCESS)
	    {
	        sprintf(info_buf, "Non-zero return code (%d) from MPI_Wait(Isend)", ierr);
	        MPITEST_message(MPITEST_NONFATAL, info_buf);
	        MPI_Error_string(ierr, error_string, &size);
	        MPITEST_message(MPITEST_FATAL, error_string);
	        fail++;
            }       /* Error Test  */
	}
    }
    return(fail);

}

/*---------------------------------------------------------------------------*/
int main(int argc, char *argv[])
{
    int
	 fail,          /* Counts number of test failures  */
	 loop_cnt,      /* Counts number of tests executed */
	 loop_fail,
	 ierr,          /* Return value from MPI calls     */
	 ub,		/* Maximum valid tag               */
	*ub_p,          /* Pointer to Maximum valid tag    */
	 size,
	 k,
	 partner,	/* Partner rank for on-going msgs. */
	 side,		/* Even or odd partner             */
	 value,	
	*value_ary,	/* Parameters and buffers for calls*/
	*value_dsplsary,
	 expect,
	*counts_ary,
	 result,
	*result_ary,
	*result_dsplsary,
	 send_buf[MPITEST_SEND_LENGTH],
	 recv_buf[MPITEST_SEND_LENGTH];

    char
	  info_buf[256],/* buffer for passing mesages to MPITEST  */
	  testname[64]; /* the name of this test           */
    char error_string[MPI_MAX_ERROR_STRING];

    MPI_Request
	 rq1,
	 rq2;

    /*
     * Initialize MPI
     */
    ierr = MPI_Init (&argc, &argv);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr);
	MPITEST_message(MPITEST_FATAL, info_buf);
    }


    sprintf(testname, "MPI_collective_message");

    /*-----------------------------  MPITEST_init  --------------------------*/
    MPITEST_init(argc, argv);
    if (MPITEST_me == 0)
    {
	sprintf(info_buf, "Starting %s test", testname);
	MPITEST_message(MPITEST_INFO0, info_buf);
    }

    if (MPITEST_nump < 2)
    {
	MPITEST_message(MPITEST_FATAL, "This test requires at least 2 ranks to run");
    }

    /* set the global error counter */
    fail = 0;
    loop_cnt = 0;
    if ((MPITEST_nump%2 == 1) && (MPITEST_me == MPITEST_nump - 1))
	partner = MPI_UNDEFINED;
    else if (MPITEST_me%2 == 0)
	partner = MPITEST_me + 1;
    else
	partner = MPITEST_me - 1;
    side = MPITEST_me%2;

    /* Reuse ub as the flag on value-is-available; if so, 
       get the upper bound */
    ierr = MPI_Attr_get(MPI_COMM_WORLD, MPI_TAG_UB, &ub_p, &ub);
    if (ub) ub   = *ub_p;

    /*
     * Allocate memory for the various collective structures
    */
    value_ary = (int *)malloc(MPITEST_nump * sizeof(int));
    value_dsplsary = (int *)malloc(MPITEST_nump * sizeof(int));
    result_ary = (int *)malloc(MPITEST_nump * sizeof(int));
    result_dsplsary = (int *)malloc(MPITEST_nump * sizeof(int));
    counts_ary = (int *)malloc(MPITEST_nump * sizeof(int));
    if ((value_ary == 0)  || (result_ary == 0) ||
	(value_dsplsary == 0) || (result_dsplsary == 0) ||
	(counts_ary == 0))
    {
	MPITEST_message(MPITEST_FATAL, "Unable to malloc memory for this test");
    }

    for (k=0; k<MPITEST_nump; k++)
    {
	value_dsplsary[k] = k;
	result_dsplsary[k] = k;
	counts_ary[k] = 1;
	
    }

    /* ------------------------------------------------------------ */
	if (MPITEST_me == 0)
    {
	sprintf(info_buf, "Allgather");
	MPITEST_message(MPITEST_INFO1, info_buf);
    }

    value = -1;
    for (k=0; k < MPITEST_nump; k++)
	result_ary[k] = 1;

    fail += precoll(partner, side, &send_buf[0], &recv_buf[0], ub/1, &rq1, &rq2);
    ierr =  MPI_Allgather(&value, 1, MPI_INT, result_ary, 1, MPI_INT, MPI_COMM_WORLD);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Allgather", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_FATAL, error_string);
	fail++;
    }       /* Error Test  */
    fail += postcoll(partner, side, &send_buf[0], &recv_buf[0], ub/1, &rq1, &rq2);


    loop_fail = 0;
    for (k=0; k < MPITEST_nump; k++)
	if (result_ary[k] != -1)
	    loop_fail++;
    if (loop_fail != 0)
    {
	sprintf(info_buf, "MPI_Allgather received %d bad values", loop_fail);
   	MPITEST_message(MPITEST_NONFATAL, info_buf);
   	fail++;
    }
    loop_cnt+=3;

    /* ------------------------------------------------------------ */
    if (MPITEST_me == 0)
    {
	sprintf(info_buf, "Allgatherv");
	MPITEST_message(MPITEST_INFO1, info_buf);
    }

    value = -1;
    for (k=0; k < MPITEST_nump; k++)
    {
	result_ary[k] = 0;
    }

    fail += precoll(partner, side, &send_buf[0], &recv_buf[0], ub/2, &rq1, &rq2);
    ierr =  MPI_Allgatherv(&value, 1, MPI_INT, result_ary, counts_ary, result_dsplsary, MPI_INT, MPI_COMM_WORLD);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Allgatherv", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_FATAL, error_string);
	fail++;
    }       /* Error Test  */
    fail += postcoll(partner, side, &send_buf[0], &recv_buf[0], ub/2, &rq1, &rq2);


    loop_fail = 0;
    for (k=0; k < MPITEST_nump; k++)
	if (result_ary[k] != -1)
	    loop_fail++;
    if (loop_fail != 0)
    {
	sprintf(info_buf, "MPI_Allgatherv received %d bad values", loop_fail);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	fail++;
    }
    loop_cnt+=3;


    /* ------------------------------------------------------------ */
    if (MPITEST_me == 0)
    {
	sprintf(info_buf, "Allreduce");
	MPITEST_message(MPITEST_INFO1, info_buf);
    }

    value = -1;
    result = 1;


    fail += precoll(partner, side, &send_buf[0], &recv_buf[0], ub/3, &rq1, &rq2);
    ierr =  MPI_Allreduce(&value, &result, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Allreduce", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_FATAL, error_string);
	fail++;
    }       /* Error Test  */
    fail += postcoll(partner, side, &send_buf[0], &recv_buf[0], ub/3, &rq1, &rq2);


    if (result != -1*MPITEST_nump)
    {
	sprintf(info_buf, "MPI_Allreduce received %d, expected %d", result, -1*MPITEST_nump);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	fail++;
    }
    loop_cnt+=3;


    /* ------------------------------------------------------------ */
    if (MPITEST_me == 0)
    {
	sprintf(info_buf, "Alltoall");
	MPITEST_message(MPITEST_INFO1, info_buf);
    }

    for (k=0; k < MPITEST_nump; k++)
    {
	value_ary[k] = -1;
	result_ary[k] = 1;
    }


    fail += precoll(partner, side, &send_buf[0], &recv_buf[0], ub/4, &rq1, &rq2);
    ierr =  MPI_Alltoall(value_ary, 1, MPI_INT, result_ary, 1, MPI_INT, MPI_COMM_WORLD);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Alltoall", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_FATAL, error_string);
	fail++;
    }       /* Error Test  */
    fail += postcoll(partner, side, &send_buf[0], &recv_buf[0], ub/4, &rq1, &rq2);


    loop_fail = 0;
    for (k=0; k < MPITEST_nump; k++)
	if (result_ary[k] != -1)
	    loop_fail++;
    if (loop_fail != 0)
    {
	sprintf(info_buf, "MPI_Alltoall received %d bad values", loop_fail);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	fail++;
    }
    loop_cnt+=3;

    /* ------------------------------------------------------------ */
    if (MPITEST_me == 0)
    {
	sprintf(info_buf, "Alltoallv");
	MPITEST_message(MPITEST_INFO1, info_buf);
    }

    for (k=0; k < MPITEST_nump; k++)
    {
	value_ary[k] = -1;
	result_ary[k] = 1;
    }


    fail += precoll(partner, side, &send_buf[0], &recv_buf[0], ub/5, &rq1, &rq2);
    ierr =  MPI_Alltoallv(value_ary, counts_ary, value_dsplsary, MPI_INT, result_ary, counts_ary, result_dsplsary, MPI_INT, MPI_COMM_WORLD);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Alltoallv", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_FATAL, error_string);
	fail++;
    }       /* Error Test  */
    fail += postcoll(partner, side, &send_buf[0], &recv_buf[0], ub/5, &rq1, &rq2);


    loop_fail = 0;
    for (k=0; k < MPITEST_nump; k++)
	if (result_ary[k] != -1)
	    loop_fail++;
    if (loop_fail != 0)
    {
	sprintf(info_buf, "MPI_Alltoallv received %d bad values", loop_fail);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	fail++;
    }
    loop_cnt+=3;

    /* ------------------------------------------------------------ */
    if (MPITEST_me == 0)
    {
	sprintf(info_buf, "Barrier");
	MPITEST_message(MPITEST_INFO1, info_buf);
    }

    fail += precoll(partner, side, &send_buf[0], &recv_buf[0], ub/6, &rq1, &rq2);
    ierr =  MPI_Barrier(MPI_COMM_WORLD);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Barrier", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_FATAL, error_string);
	fail++;
    }       /* Error Test  */
    fail += postcoll(partner, side, &send_buf[0], &recv_buf[0], ub/6, &rq1, &rq2);


    loop_cnt+=3;

    /* ------------------------------------------------------------ */
    if (MPITEST_me == 0)
    {
	sprintf(info_buf, "Bcast");
	MPITEST_message(MPITEST_INFO1, info_buf);
    }

    value = MPITEST_me;


    fail += precoll(partner, side, &send_buf[0], &recv_buf[0], ub/7, &rq1, &rq2);
    ierr =  MPI_Bcast(&value, 1, MPI_INT, 1, MPI_COMM_WORLD);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Bcast", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_FATAL, error_string);
	fail++;
    }       /* Error Test  */
    fail += postcoll(partner, side, &send_buf[0], &recv_buf[0], ub/7, &rq1, &rq2);


    if (value != 1)
    {
	sprintf(info_buf, "MPI_Bcast received %d, expected 1", value);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	fail++;
    }
    loop_cnt+=3;

    /* ------------------------------------------------------------ */
    if (MPITEST_me == 0)
    {
	sprintf(info_buf, "Gather");
	MPITEST_message(MPITEST_INFO1, info_buf);
    }

    value = -1;
    for (k=0; k < MPITEST_nump; k++)
	result_ary[k] = 1;


    fail += precoll(partner, side, &send_buf[0], &recv_buf[0], ub/8, &rq1, &rq2);
    ierr =  MPI_Gather(&value, 1, MPI_INT, result_ary, 1, MPI_INT, 1, MPI_COMM_WORLD);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Gather", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_FATAL, error_string);
	fail++;
    }       /* Error Test  */
    fail += postcoll(partner, side, &send_buf[0], &recv_buf[0], ub/8, &rq1, &rq2);


    loop_fail = 0;
    if (MPITEST_me == 1)
    {
	for (k=0; k < MPITEST_nump; k++)
	    if (result_ary[k] != -1)
		loop_fail++;
	}
    if (loop_fail != 0)
    {
	sprintf(info_buf, "MPI_Gather received %d bad values", loop_fail);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	fail++;
    }
    loop_cnt+=3;

    /* ------------------------------------------------------------ */
    if (MPITEST_me == 0)
    {
	sprintf(info_buf, "Gatherv");
	MPITEST_message(MPITEST_INFO1, info_buf);
    }

    value = -1;
    for (k=0; k < MPITEST_nump; k++)
	result_ary[k] = 1;


    fail += precoll(partner, side, &send_buf[0], &recv_buf[0], ub/9, &rq1, &rq2);
    ierr =  MPI_Gatherv(&value, 1, MPI_INT, result_ary, counts_ary, result_dsplsary, MPI_INT, 1, MPI_COMM_WORLD);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Gatherv", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_FATAL, error_string);
	fail++;
    }       /* Error Test  */
    fail += postcoll(partner, side, &send_buf[0], &recv_buf[0], ub/9, &rq1, &rq2);


    loop_fail = 0;
    if (MPITEST_me == 1)
    {
	for (k=0; k < MPITEST_nump; k++)
	    if (result_ary[k] != -1)
		loop_fail++;
    }
    if (loop_fail != 0)
    {
	sprintf(info_buf, "MPI_Gatherv received %d bad values", loop_fail);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	fail++;
    }
    loop_cnt+=3;

    /* ------------------------------------------------------------ */
    if (MPITEST_me == 0)
    {
	sprintf(info_buf, "Reduce");
	MPITEST_message(MPITEST_INFO1, info_buf);
    }

    value = -1;
    result = 1;


    fail += precoll(partner, side, &send_buf[0], &recv_buf[0], ub/10, &rq1, &rq2);
    ierr =  MPI_Reduce(&value, &result, 1, MPI_INT, MPI_SUM, 1, MPI_COMM_WORLD);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Reduce", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_FATAL, error_string);
	fail++;
    }       /* Error Test  */
    fail += postcoll(partner, side, &send_buf[0], &recv_buf[0], ub/10, &rq1, &rq2);


    if ((MPITEST_me == 1) && (result != -1*MPITEST_nump) )
    {
	sprintf(info_buf, "MPI_Reduce received %d, expected %d", result, MPITEST_nump);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	fail++;
    }
    loop_cnt+=3;

    /* ------------------------------------------------------------ */
    if (MPITEST_me == 0)
    {
	sprintf(info_buf, "Reduce_scatter");
	MPITEST_message(MPITEST_INFO1, info_buf);
    }

    for (k=0; k < MPITEST_nump; k++)
	value_ary[k] = -1;
    result = 7;


    fail += precoll(partner, side, &send_buf[0], &recv_buf[0], ub/11, &rq1, &rq2);
    ierr =  MPI_Reduce_scatter(value_ary, &result, counts_ary, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Reduce_scatter", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_FATAL, error_string);
	fail++;
    }       /* Error Test  */
    fail += postcoll(partner, side, &send_buf[0], &recv_buf[0], ub/11, &rq1, &rq2);


    if (result != -1*MPITEST_nump)
    {
	sprintf(info_buf, "MPI_Reduce_scatter received %d expected %d", result, -1*MPITEST_nump);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	fail++;
    }
    loop_cnt+=3;

    /* ------------------------------------------------------------ */
    if (MPITEST_me == 0)
    {
	sprintf(info_buf, "Scan");
	MPITEST_message(MPITEST_INFO1, info_buf);
    }

    value = -1;
    result = 1;


    fail += precoll(partner, side, &send_buf[0], &recv_buf[0], ub/12, &rq1, &rq2);
    ierr =  MPI_Scan(&value, &result, 1, MPI_INT, MPI_SUM, MPI_COMM_WORLD);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Scan", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_FATAL, error_string);
	fail++;
    }       /* Error Test  */
    fail += postcoll(partner, side, &send_buf[0], &recv_buf[0], ub/12, &rq1, &rq2);


    if (result != -1*MPITEST_me-1)
    {
	sprintf(info_buf, "MPI_Scan received %d, expected %d", result, -1*MPITEST_me-1);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	fail++;
    }
    loop_cnt+=3;

    /* ------------------------------------------------------------ */
    if (MPITEST_me == 0)
    {
	sprintf(info_buf, "Scatter");
	MPITEST_message(MPITEST_INFO1, info_buf);
    }

    if (MPITEST_me == 1)
    {
	for (k=0; k < MPITEST_nump; k++)
	    value_ary[k] = -1;
    }
    result = 7;


    fail += precoll(partner, side, &send_buf[0], &recv_buf[0], ub/13, &rq1, &rq2);
    ierr =  MPI_Scatter(value_ary, 1, MPI_INT, &result, 1, MPI_INT, 1, MPI_COMM_WORLD);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Scatter", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_FATAL, error_string);
	fail++;
    }       /* Error Test  */
    fail += postcoll(partner, side, &send_buf[0], &recv_buf[0], ub/13, &rq1, &rq2);


    if (result != -1)
    {
	sprintf(info_buf, "MPI_Scatter received %d expected -1", result);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	fail++;
    }
    loop_cnt+=3;

    /* ------------------------------------------------------------ */
    if (MPITEST_me == 0)
    {
	sprintf(info_buf, "Scatterv");
	MPITEST_message(MPITEST_INFO1, info_buf);
    }

    if (MPITEST_me == 1)
    {
	for (k=0; k < MPITEST_nump; k++)
	    value_ary[k] = -1;
    }
    result = 7;


    fail += precoll(partner, side, &send_buf[0], &recv_buf[0], 0, &rq1, &rq2);
    ierr =  MPI_Scatterv(value_ary, counts_ary, value_dsplsary, MPI_INT, &result, 1, MPI_INT, 1, MPI_COMM_WORLD);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Scatterv", ierr);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	MPI_Error_string(ierr, error_string, &size);
	MPITEST_message(MPITEST_FATAL, error_string);
	fail++;
    }       /* Error Test  */
    fail += postcoll(partner, side, &send_buf[0], &recv_buf[0], 0, &rq1, &rq2);


    if (result != -1)
    {
	sprintf(info_buf, "MPI_Scatterv received %d expected -1", result);
	MPITEST_message(MPITEST_NONFATAL, info_buf);
	fail++;
    }
    loop_cnt+=3;

    /* ------------------------------------------------------------ */

    free(result_ary);
    free(result_dsplsary);
    free(value_ary);
    free(value_dsplsary);
    free(counts_ary);

    /* report overall results  */

    MPITEST_report(loop_cnt - fail, fail, 0, testname);

    MPI_Finalize();

    return fail;

}/* main() */

