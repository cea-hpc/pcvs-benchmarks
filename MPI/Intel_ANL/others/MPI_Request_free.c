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
		  Test for MPI_Request_free()

This test verifies that MPI_Request_free can be used on each of the basic
non-blocking calls.  Requests are expected to complete even though they are
freed.

This test uses the first 2 ranks in MPI_COMM_WORLD, with rank 0 making 
the non-blocking and free calls and rank1 as the destination.
******************************************************************************/

#include "mpitest_cfg.h"
#include "mpitest.h"


int main(int argc, char *argv[])
{

	int
          loop_cnt,         /* counts total number of times through loop     */
          fail,   	    /* counts total number of failures               */
          ierr,	            /* return value from MPI calls                   */
          size,	            /* return size from MPI_Error_string             */
	  i;

	int
          isendbuf[1],
          issendbuf[1],
          irsendbuf[1],
          ibsendbuf[1],
	  recvbuf[5];

        char
	  buffer[2000+MPI_BSEND_OVERHEAD];

        char
	  info_buf[256],    /* buffer for passing mesages to MPITEST         */
	  testname[64];     /* the name of this test                         */
        char error_string[MPI_MAX_ERROR_STRING];

        MPI_Status
          recv_stat[5],
	  send_stat;	    /* MPI  status structure                         */

        MPI_Request
	  recv_request[5],  /*  MPI request structure                        */
	  send_request;     /*  MPI request structure                        */

    /*-----------------------------  MPI_Init  ------------------------------*/
    ierr = MPI_Init(&argc, &argv);
    if (ierr != MPI_SUCCESS)
    {
	sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()", ierr);
	MPITEST_message(MPITEST_FATAL, info_buf);
    }


    sprintf(testname, "MPI_Request_free");

    /*-----------------------------  MPITEST_init  --------------------------*/
    MPITEST_init(argc, argv);
    if (MPITEST_me == 0)
    {
	sprintf(info_buf, "Starting %s test", testname);
	MPITEST_message(MPITEST_INFO0, info_buf);
    }

    if (MPITEST_nump < 2)
    {
	sprintf(info_buf, "At least 2 ranks required to run this test");
	MPITEST_message(MPITEST_FATAL, info_buf);
    }

    /* set the global error counter */
    loop_cnt = 0;
    fail = 0;

    if (MPITEST_me < 2)
    {

	if (MPITEST_me == 0)
	{ /* sender */
	    ierr = MPI_Barrier(MPI_COMM_WORLD);
	    ierr = MPI_Buffer_attach(buffer, 2000+MPI_BSEND_OVERHEAD); 

	    loop_cnt++;
	    ierr = MPI_Isend(&isendbuf, 1, MPI_INT, 1, 1,
			MPI_COMM_WORLD,  &send_request);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Isend", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }	/* Isend Error Test  */


            ierr = MPI_Request_free(&send_request);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Request_free on Isend", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	        MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	     }  /* Wait Error Test  */
	    if (send_request != MPI_REQUEST_NULL)
	    {
		sprintf(info_buf, "Request not set to MPI_REQUEST_NULL by MPI_Request_free on Isend");
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }

	    loop_cnt++;
	    ierr = MPI_Ibsend(&ibsendbuf, 1, MPI_INT, 1, 2,
			MPI_COMM_WORLD,  &send_request);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Ibsend", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }	/* Ibsend Error Test  */


            ierr = MPI_Request_free(&send_request);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Request_free on Ibsend", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	        MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	     }  /* Wait Error Test  */
	    if (send_request != MPI_REQUEST_NULL)
	    {
		sprintf(info_buf, "Request not set to MPI_REQUEST_NULL by MPI_Request_free on Ibsend");
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }

	    loop_cnt++;
	    ierr = MPI_Issend(&issendbuf, 1, MPI_INT, 1, 3,
			MPI_COMM_WORLD,  &send_request);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Issend", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }	/* Issend Error Test  */


            ierr = MPI_Request_free(&send_request);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Request_free on Issend", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	        MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	     }  /* Wait Error Test  */
	    if (send_request != MPI_REQUEST_NULL)
	    {
		sprintf(info_buf, "Request not set to MPI_REQUEST_NULL by MPI_Request_free on Issend");
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }

	    loop_cnt++;
	    ierr = MPI_Irsend(&irsendbuf, 1, MPI_INT, 1, 4,
			MPI_COMM_WORLD,  &send_request);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Irsend", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }	/* Irsend Error Test  */


            ierr = MPI_Request_free(&send_request);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Request_free on Irsend", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	        MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }  /* Wait Error Test  */
	    if (send_request != MPI_REQUEST_NULL)
	    {
		sprintf(info_buf, "Request not set to MPI_REQUEST_NULL by MPI_Request_free on Irsend");
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		fail++;
	    }

	    loop_cnt++;
	    ierr = MPI_Isend(&isendbuf, 1, MPI_INT, 1, 5,
			MPI_COMM_WORLD,  &send_request);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Isend", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
		MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	    }	/* Isend Error Test  */


            ierr = MPI_Wait(&send_request, &send_stat);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Wait on Isend", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	        MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	     }  /* Wait Error Test  */

	    {char *bbuf; int bsize; 
	    MPI_Buffer_detach( &bbuf, &bsize ); }
	} /* sender */

	else

	{ /* receiver */
	    for (i=0; i<5; i++)
	    {
		loop_cnt++;
	        ierr = MPI_Irecv(&recvbuf[i], 1, MPI_INT, 0, i+1,
			MPI_COMM_WORLD, &recv_request[i]);
	        if (ierr != MPI_SUCCESS)
	        {
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_Irecv #%d", ierr, i);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
		    MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_NONFATAL, error_string);
		    fail++;
	        }	/* Irecv  Error Test  */
	    }
	    ierr = MPI_Barrier(MPI_COMM_WORLD);

	    for (i=0; i<4; i++)
	    {
                ierr = MPI_Wait(&recv_request[i], &recv_stat[i]);
	        if (ierr != MPI_SUCCESS)
	        {
		    sprintf(info_buf, "Non-zero return code (%d) from MPI_Wait on Irecv #%d", ierr, i);
		    MPITEST_message(MPITEST_NONFATAL, info_buf);
	            MPI_Error_string(ierr, error_string, &size);
		    MPITEST_message(MPITEST_NONFATAL, error_string);
		    fail++;
	         }  /* Wait Error Test  */
	    }


            ierr = MPI_Request_free(&recv_request[4]);
	    if (ierr != MPI_SUCCESS)
	    {
		sprintf(info_buf, "Non-zero return code (%d) from MPI_Request_free on Irecv", ierr);
		MPITEST_message(MPITEST_NONFATAL, info_buf);
	        MPI_Error_string(ierr, error_string, &size);
		MPITEST_message(MPITEST_NONFATAL, error_string);
		fail++;
	     }  /* Wait Error Test  */



	} /* receiver */

    }
    else
    {   /* rank >= 2 need to match Barrier above */
	MPI_Barrier(MPI_COMM_WORLD);
    }

    /* report overall results  */

    MPITEST_report(loop_cnt - fail, fail, 0, testname);

    MPI_Finalize();

    return fail;

}/* main() */
